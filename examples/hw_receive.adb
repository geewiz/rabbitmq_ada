--  hw_receive.adb - Subscribes to "Hello World" messages and prints them
--
--  Usage: Run this program to receive messages from the "hello" fanout
--  exchange. Multiple receivers can run simultaneously.

with Ada.Text_IO;
with Ada.Strings.Unbounded;

with RabbitMQ.Connections;
with RabbitMQ.Channels;
with RabbitMQ.Exchanges;
with RabbitMQ.Queues;
with RabbitMQ.Consuming;
with RabbitMQ.Exceptions;

procedure HW_Receive is
   use Ada.Text_IO;
   use Ada.Strings.Unbounded;

   Conn : RabbitMQ.Connections.Connection;
   Ch   : RabbitMQ.Channels.Channel;

   Exchange_Name : constant String := "hello";
begin
   Put_Line ("Connecting to RabbitMQ...");

   RabbitMQ.Connections.Connect
     (Conn         => Conn,
      Host         => "localhost",
      Port         => 5672,
      User         => "guest",
      Password     => "guest",
      Virtual_Host => "/");

   Put_Line ("Connected. Opening channel...");

   RabbitMQ.Channels.Open (Ch, Conn, Number => 1);

   Put_Line ("Channel opened. Declaring exchange...");

   RabbitMQ.Exchanges.Declare_Exchange
     (Ch          => Ch,
      Name        => Exchange_Name,
      Kind        => RabbitMQ.Exchanges.Fanout,
      Durable     => False,
      Auto_Delete => False);

   Put_Line ("Exchange '" & Exchange_Name & "' declared.");

   --  Declare an exclusive, auto-delete queue (broker generates the name)
   declare
      Queue_Info : constant RabbitMQ.Queues.Queue_Info :=
        RabbitMQ.Queues.Declare_Queue
          (Ch          => Ch,
           Name        => "",
           Durable     => False,
           Exclusive   => True,
           Auto_Delete => True);

      Queue_Name : constant String :=
        Queue_Info.Name (1 .. Queue_Info.Name_Length);

      Consumer_Tag : constant String :=
        RabbitMQ.Consuming.Start_Consumer
          (Ch     => Ch,
           Queue  => Queue_Name,
           No_Ack => True);

      Delivery : RabbitMQ.Consuming.Delivery;
      Got_Msg  : Boolean;
   begin
      Put_Line ("Queue '" & Queue_Name & "' declared.");

      --  Bind queue to the fanout exchange
      RabbitMQ.Queues.Bind
        (Ch          => Ch,
         Queue       => Queue_Name,
         Exchange    => Exchange_Name,
         Routing_Key => "");

      Put_Line ("Queue bound to exchange.");
      Put_Line ("Consumer started: " & Consumer_Tag);
      Put_Line ("Waiting for messages (Ctrl+C to stop)...");
      New_Line;

      --  Consume messages forever
      loop
         Got_Msg := RabbitMQ.Consuming.Consume_Message
           (Ch         => Ch,
            Msg        => Delivery,
            Timeout_Ms => 0);

         if Got_Msg then
            Put_Line ("[x] Received: " &
              To_String (Delivery.Message.Content));
         end if;
      end loop;
   end;

exception
   when RabbitMQ.Exceptions.Connection_Error =>
      Put_Line ("ERROR: Failed to connect to RabbitMQ.");
      Put_Line ("Make sure RabbitMQ is running on localhost:5672");

   when RabbitMQ.Exceptions.Channel_Error =>
      Put_Line ("ERROR: Channel operation failed.");
end HW_Receive;
