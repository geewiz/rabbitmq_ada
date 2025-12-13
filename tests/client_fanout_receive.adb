--  client_fanout_receive.adb - Fanout subscriber using RabbitMQ.Client
--
--  Usage: Run multiple instances of this program. Each will receive
--  ALL messages broadcast to the fanout exchange.

with Ada.Text_IO;

with RabbitMQ.Client;
with RabbitMQ.Exchanges;
with RabbitMQ.Exceptions;
with RabbitMQ.Messages;

procedure Client_Fanout_Receive is
   use Ada.Text_IO;

   procedure On_Message (Msg : RabbitMQ.Messages.Message) is
   begin
      Put_Line ("[x] Received: " & RabbitMQ.Messages.Get_Content (Msg));
   end On_Message;

   C : RabbitMQ.Client.Connection;
begin
   Put_Line ("Connecting to RabbitMQ...");

   RabbitMQ.Client.Connect (C, "amqp://guest:guest@localhost");

   Put_Line ("Connected. Declaring fanout exchange...");

   RabbitMQ.Client.Declare_Exchange
     (C        => C,
      Exchange => "broadcast",
      Kind     => RabbitMQ.Exchanges.Fanout);

   Put_Line ("Exchange 'broadcast' declared.");

   --  Create exclusive temporary queue (broker assigns name)
   declare
      Queue_Name : constant String := RabbitMQ.Client.Declare_Queue (C);
   begin
      Put_Line ("Temporary queue '" & Queue_Name & "' created.");

      --  Bind our queue to the fanout exchange
      RabbitMQ.Client.Bind_Queue
        (C        => C,
         Queue    => Queue_Name,
         Exchange => "broadcast");

      Put_Line ("Queue bound to exchange.");
      Put_Line ("Waiting for broadcasts (Ctrl+C to stop)...");
      New_Line;

      --  Subscribe blocks until Unsubscribe is called or connection closes
      RabbitMQ.Client.Subscribe
        (C       => C,
         Queue   => Queue_Name,
         Handler => On_Message'Unrestricted_Access);
   end;

exception
   when RabbitMQ.Exceptions.Invalid_URL =>
      Put_Line ("ERROR: Invalid AMQP URL.");

   when RabbitMQ.Exceptions.Connection_Error =>
      Put_Line ("ERROR: Failed to connect to RabbitMQ.");
      Put_Line ("Make sure RabbitMQ is running on localhost:5672");

   when RabbitMQ.Exceptions.Channel_Error =>
      Put_Line ("ERROR: Channel operation failed.");
end Client_Fanout_Receive;
