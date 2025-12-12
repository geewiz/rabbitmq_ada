--  Test program for RabbitMQ bindings

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Strings.Unbounded;
with RabbitMQ.Connections;
with RabbitMQ.Channels;
with RabbitMQ.Queues;
with RabbitMQ.Exchanges;
with RabbitMQ.Messages;
with RabbitMQ.Publishing;
with RabbitMQ.Consuming;
with RabbitMQ.Exceptions;

procedure Test_Connection is
   use Ada.Text_IO;
   package SU renames Ada.Strings.Unbounded;

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

      --  Test publishing
      Put_Line ("5. Testing Message Publishing");

      --  Create a queue to publish to
      Put_Line ("   Creating test queue for publishing...");
      declare
         Info : constant RabbitMQ.Queues.Queue_Info :=
           RabbitMQ.Queues.Declare_Queue
             (Ch          => Ch,
              Name        => "publish-test-queue",
              Durable     => False,
              Exclusive   => False,
              Auto_Delete => True);
         pragma Unreferenced (Info);
      begin
         Put_Line ("   SUCCESS: Queue created");
      end;

      --  Publish a simple string message
      Put_Line ("   Publishing simple string message...");
      RabbitMQ.Publishing.Publish_To_Queue
        (Ch   => Ch,
         Queue => "publish-test-queue",
         Data  => "Hello, RabbitMQ from Ada!");
      Put_Line ("   SUCCESS: Simple message published");

      --  Publish a text message
      Put_Line ("   Publishing text message...");
      RabbitMQ.Publishing.Publish_To_Queue
        (Ch      => Ch,
         Queue   => "publish-test-queue",
         Message => RabbitMQ.Messages.Text_Message ("This is a text message"));
      Put_Line ("   SUCCESS: Text message published");

      --  Publish a persistent message
      Put_Line ("   Publishing persistent message...");
      RabbitMQ.Publishing.Publish_To_Queue
        (Ch      => Ch,
         Queue   => "publish-test-queue",
         Message => RabbitMQ.Messages.Persistent_Message
                      ("This message survives restarts", "text/plain"));
      Put_Line ("   SUCCESS: Persistent message published");

      --  Publish with custom properties
      Put_Line ("   Publishing message with custom properties...");
      declare
         use RabbitMQ.Messages;
         Msg : Message;
      begin
         Msg.Content := SU.To_Unbounded_String ("Message with properties");
         Msg.Properties.Content_Type :=
           SU.To_Unbounded_String ("application/json");
         Msg.Properties.Correlation_Id :=
           SU.To_Unbounded_String ("req-12345");
         Msg.Properties.Reply_To :=
           SU.To_Unbounded_String ("reply-queue");
         Msg.Properties.Priority := 5;
         Msg.Properties.Delivery_Mode := Persistent;

         RabbitMQ.Publishing.Publish_To_Queue
           (Ch      => Ch,
            Queue   => "publish-test-queue",
            Message => Msg);
         Put_Line ("   SUCCESS: Message with properties published");
      end;

      --  Check message count in queue
      Put_Line ("   Verifying messages in queue...");
      declare
         Info : constant RabbitMQ.Queues.Queue_Info :=
           RabbitMQ.Queues.Declare_Queue
             (Ch      => Ch,
              Name    => "publish-test-queue",
              Passive => True);
      begin
         Put_Line ("   Queue has" & Info.Message_Count'Image & " messages");
         if Info.Message_Count = 4 then
            Put_Line ("   SUCCESS: All 4 messages are in the queue!");
         else
            Put_Line ("   WARNING: Expected 4 messages");
         end if;
      end;

      --  Clean up the publish test queue
      Put_Line ("   Cleaning up publish test queue...");
      declare
         Deleted : constant Natural :=
           RabbitMQ.Queues.Delete (Ch, "publish-test-queue");
      begin
         Put_Line ("   SUCCESS: Deleted queue with" &
                   Deleted'Image & " messages");
      end;

      New_Line;

      --  Test consuming
      Put_Line ("6. Testing Message Consuming");

      --  Create a queue and publish some messages to consume
      Put_Line ("   Creating test queue for consuming...");
      declare
         Info : constant RabbitMQ.Queues.Queue_Info :=
           RabbitMQ.Queues.Declare_Queue
             (Ch          => Ch,
              Name        => "consume-test-queue",
              Durable     => False,
              Exclusive   => False,
              Auto_Delete => True);
         pragma Unreferenced (Info);
      begin
         Put_Line ("   SUCCESS: Queue created");
      end;

      --  Publish test messages
      Put_Line ("   Publishing 3 test messages...");
      for I in 1 .. 3 loop
         RabbitMQ.Publishing.Publish_To_Queue
           (Ch    => Ch,
            Queue => "consume-test-queue",
            Data  => "Test message" & I'Image);
      end loop;
      Put_Line ("   SUCCESS: Messages published");

      --  Test basic.get (polling)
      Put_Line ("   Testing Get_Message (polling)...");
      declare
         Delivery : RabbitMQ.Consuming.Delivery;
         Got_Msg  : Boolean;
      begin
         Got_Msg := RabbitMQ.Consuming.Get_Message
           (Ch     => Ch,
            Queue  => "consume-test-queue",
            Msg    => Delivery,
            No_Ack => True);

         if Got_Msg then
            Put_Line ("   SUCCESS: Got message: " &
                      SU.To_String (Delivery.Message.Content));
         else
            Put_Line ("   FAILED: No message received");
         end if;
      end;

      --  Test consumer with consume_message
      Put_Line ("   Testing Start_Consumer and Consume_Message...");
      declare
         Consumer_Tag : constant String :=
           RabbitMQ.Consuming.Start_Consumer
             (Ch     => Ch,
              Queue  => "consume-test-queue",
              No_Ack => False);
         Delivery : RabbitMQ.Consuming.Delivery;
         Got_Msg  : Boolean;
      begin
         Put_Line ("   Consumer started with tag: " & Consumer_Tag);

         --  Consume remaining messages (should be 2)
         for I in 1 .. 2 loop
            Got_Msg := RabbitMQ.Consuming.Consume_Message
              (Ch         => Ch,
               Msg        => Delivery,
               Timeout_Ms => 5000);

            if Got_Msg then
               Put_Line ("   Got message: " &
                         SU.To_String (Delivery.Message.Content));
               --  Acknowledge the message
               RabbitMQ.Consuming.Ack
                 (Ch           => Ch,
                  Delivery_Tag => Delivery.Delivery_Tag);
               Put_Line ("   Acknowledged message");
            else
               Put_Line ("   Timeout waiting for message");
            end if;
         end loop;

         --  Cancel the consumer
         Put_Line ("   Cancelling consumer...");
         RabbitMQ.Consuming.Cancel_Consumer (Ch, Consumer_Tag);
         Put_Line ("   SUCCESS: Consumer cancelled");
      end;

      --  Clean up the consume test queue
      Put_Line ("   Cleaning up consume test queue...");
      declare
         Deleted : constant Natural :=
           RabbitMQ.Queues.Delete (Ch, "consume-test-queue");
      begin
         Put_Line ("   SUCCESS: Deleted queue with" &
                   Deleted'Image & " messages remaining");
      end;

      New_Line;

      --  Clean up
      Put_Line ("7. Cleanup");
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
