--  client_fanout_send.adb - Fanout publisher using RabbitMQ.Client
--
--  Usage: Run this program to broadcast messages to all subscribers
--  via a fanout exchange. All connected receivers will get every message.

with Ada.Text_IO;

with RabbitMQ.Client;
with RabbitMQ.Exchanges;
with RabbitMQ.Exceptions;

procedure Client_Fanout_Send is
   use Ada.Text_IO;

   C       : RabbitMQ.Client.Connection;
   Counter : Natural := 0;
begin
   Put_Line ("Connecting to RabbitMQ...");

   RabbitMQ.Client.Connect (C, "amqp://guest:guest@localhost");

   Put_Line ("Connected. Declaring fanout exchange...");

   RabbitMQ.Client.Declare_Exchange
     (C        => C,
      Exchange => "broadcast",
      Kind     => RabbitMQ.Exchanges.Fanout);

   Put_Line ("Exchange 'broadcast' declared.");
   Put_Line ("Broadcasting messages (Ctrl+C to stop)...");
   New_Line;

   loop
      Counter := Counter + 1;

      declare
         Body_Text : constant String :=
           "Broadcast #" & Natural'Image (Counter);
      begin
         RabbitMQ.Client.Publish
           (C           => C,
            Exchange    => "broadcast",
            Routing_Key => "",
            Message     => Body_Text);

         Put_Line ("[x] Sent: " & Body_Text);
      end;

      delay 2.0;
   end loop;

exception
   when RabbitMQ.Exceptions.Invalid_URL =>
      Put_Line ("ERROR: Invalid AMQP URL.");

   when RabbitMQ.Exceptions.Connection_Error =>
      Put_Line ("ERROR: Failed to connect to RabbitMQ.");
      Put_Line ("Make sure RabbitMQ is running on localhost:5672");

   when RabbitMQ.Exceptions.Channel_Error =>
      Put_Line ("ERROR: Channel operation failed.");
end Client_Fanout_Send;
