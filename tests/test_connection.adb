--  Test program for RabbitMQ bindings

with Ada.Text_IO;
with Ada.Exceptions;
with RabbitMQ.Connections;
with RabbitMQ.Channels;
with RabbitMQ.Queues;
with RabbitMQ.Exchanges;
with RabbitMQ.Exceptions;

procedure Test_Connection is
   use Ada.Text_IO;

   Conn : RabbitMQ.Connections.Connection;
   Ch   : RabbitMQ.Channels.Channel;
begin
   Put_Line ("RabbitMQ Ada Bindings Test");
   Put_Line ("==========================");
   New_Line;

   begin
      --  Test connection
      Put_Line ("1. Testing Connection");
      Put_Line ("   Connecting to localhost:5672 as guest...");

      RabbitMQ.Connections.Connect
        (Conn         => Conn,
         Host         => "localhost",
         Port         => 5672,
         User         => "guest",
         Password     => "guest",
         Virtual_Host => "/");

      if RabbitMQ.Connections.Is_Open (Conn) then
         Put_Line ("   SUCCESS: Connected to RabbitMQ broker!");
      else
         Put_Line ("   FAILED: Connection reports not open");
         return;
      end if;

      New_Line;

      --  Test channel
      Put_Line ("2. Testing Channel");
      Put_Line ("   Opening channel 1...");

      RabbitMQ.Channels.Open (Ch, Conn, Number => 1);

      if RabbitMQ.Channels.Is_Open (Ch) then
         Put_Line ("   SUCCESS: Channel " &
                   RabbitMQ.Channels.Get_Number (Ch)'Image & " opened!");
      else
         Put_Line ("   FAILED: Channel reports not open");
         return;
      end if;

      New_Line;

      --  Test exchange operations
      Put_Line ("3. Testing Exchange Operations");

      Put_Line ("   Declaring exchange 'test-exchange' (direct)...");
      RabbitMQ.Exchanges.Declare_Exchange
        (Ch          => Ch,
         Name        => "test-exchange",
         Kind        => RabbitMQ.Exchanges.Direct,
         Durable     => False,
         Auto_Delete => True);
      Put_Line ("   SUCCESS: Exchange declared");

      Put_Line ("   Declaring exchange 'test-fanout' (fanout)...");
      RabbitMQ.Exchanges.Declare_Exchange
        (Ch          => Ch,
         Name        => "test-fanout",
         Kind        => RabbitMQ.Exchanges.Fanout,
         Durable     => False,
         Auto_Delete => True);
      Put_Line ("   SUCCESS: Fanout exchange declared");

      --  Test exchange-to-exchange binding
      Put_Line ("   Binding 'test-fanout' to 'test-exchange'...");
      RabbitMQ.Exchanges.Bind
        (Ch          => Ch,
         Destination => "test-fanout",
         Source      => "test-exchange",
         Routing_Key => "test-key");
      Put_Line ("   SUCCESS: Exchange bound");

      Put_Line ("   Unbinding 'test-fanout' from 'test-exchange'...");
      RabbitMQ.Exchanges.Unbind
        (Ch          => Ch,
         Destination => "test-fanout",
         Source      => "test-exchange",
         Routing_Key => "test-key");
      Put_Line ("   SUCCESS: Exchange unbound");

      Put_Line ("   Deleting exchanges...");
      RabbitMQ.Exchanges.Delete (Ch, "test-exchange");
      RabbitMQ.Exchanges.Delete (Ch, "test-fanout");
      Put_Line ("   SUCCESS: Exchanges deleted");

      New_Line;

      --  Test queue operations
      Put_Line ("4. Testing Queue Operations");

      --  Declare a named queue
      Put_Line ("   Declaring queue 'test-queue'...");
      declare
         Info : constant RabbitMQ.Queues.Queue_Info :=
           RabbitMQ.Queues.Declare_Queue
             (Ch          => Ch,
              Name        => "test-queue",
              Durable     => False,
              Exclusive   => False,
              Auto_Delete => True);
         Queue_Name : constant String :=
           Info.Name (1 .. Info.Name_Length);
      begin
         Put_Line ("   SUCCESS: Queue '" & Queue_Name & "' declared");
         Put_Line ("   Message count:" & Info.Message_Count'Image);
         Put_Line ("   Consumer count:" & Info.Consumer_Count'Image);
      end;

      --  Declare an anonymous queue (broker-generated name)
      Put_Line ("   Declaring anonymous queue...");
      declare
         Info : constant RabbitMQ.Queues.Queue_Info :=
           RabbitMQ.Queues.Declare_Queue
             (Ch        => Ch,
              Exclusive => True);
         Queue_Name : constant String :=
           Info.Name (1 .. Info.Name_Length);
      begin
         Put_Line ("   SUCCESS: Anonymous queue '" &
                   Queue_Name & "' declared");

         --  Delete the anonymous queue
         Put_Line ("   Deleting anonymous queue...");
         RabbitMQ.Queues.Delete (Ch, Queue_Name);
         Put_Line ("   SUCCESS: Queue deleted");
      end;

      --  Bind and unbind queue
      Put_Line ("   Binding 'test-queue' to 'amq.direct' exchange...");
      RabbitMQ.Queues.Bind
        (Ch          => Ch,
         Queue       => "test-queue",
         Exchange    => "amq.direct",
         Routing_Key => "test-key");
      Put_Line ("   SUCCESS: Queue bound");

      Put_Line ("   Unbinding 'test-queue' from 'amq.direct' exchange...");
      RabbitMQ.Queues.Unbind
        (Ch          => Ch,
         Queue       => "test-queue",
         Exchange    => "amq.direct",
         Routing_Key => "test-key");
      Put_Line ("   SUCCESS: Queue unbound");

      --  Purge queue
      Put_Line ("   Purging 'test-queue'...");
      declare
         Purged : constant Natural :=
           RabbitMQ.Queues.Purge (Ch, "test-queue");
      begin
         Put_Line ("   SUCCESS: Purged" & Purged'Image & " messages");
      end;

      --  Delete queue
      Put_Line ("   Deleting 'test-queue'...");
      declare
         Deleted : constant Natural :=
           RabbitMQ.Queues.Delete (Ch, "test-queue");
      begin
         Put_Line ("   SUCCESS: Deleted queue with" &
                   Deleted'Image & " messages");
      end;

      New_Line;

      --  Clean up
      Put_Line ("5. Cleanup");
      Put_Line ("   Closing channel...");
      RabbitMQ.Channels.Close (Ch);
      Put_Line ("   Channel closed.");
      Put_Line ("   Closing connection...");
      RabbitMQ.Connections.Close (Conn);
      Put_Line ("   Connection closed.");

   exception
      when E : RabbitMQ.Exceptions.Connection_Error =>
         Put_Line ("Connection error: " &
                   Ada.Exceptions.Exception_Message (E));
         Put_Line ("Make sure RabbitMQ is running on localhost:5672");

      when E : RabbitMQ.Exceptions.Channel_Error =>
         Put_Line ("Channel error: " &
                   Ada.Exceptions.Exception_Message (E));

      when E : RabbitMQ.Exceptions.Socket_Error =>
         Put_Line ("Socket error: " &
                   Ada.Exceptions.Exception_Message (E));
         Put_Line ("Make sure RabbitMQ is running on localhost:5672");

      when E : RabbitMQ.Exceptions.Protocol_Error =>
         Put_Line ("Protocol error: " &
                   Ada.Exceptions.Exception_Message (E));

      when E : others =>
         Put_Line ("Unexpected error: " &
                   Ada.Exceptions.Exception_Information (E));
   end;

   New_Line;
   Put_Line ("Test complete.");
end Test_Connection;
