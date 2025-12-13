--  client_receive.adb - Simple subscriber using RabbitMQ.Client high-level API
--
--  Usage: Run this program to receive messages from the "hello" queue
--  using the simplified Client API with callback-based consumption.

with Ada.Text_IO;

with RabbitMQ.Client;
with RabbitMQ.Exceptions;
with RabbitMQ.Messages;

procedure Client_Receive is
   use Ada.Text_IO;

   procedure On_Message (Msg : RabbitMQ.Messages.Message) is
   begin
      Put_Line ("[x] Received: " & RabbitMQ.Messages.Get_Content (Msg));
   end On_Message;

   C : RabbitMQ.Client.Connection;
begin
   Put_Line ("Connecting to RabbitMQ...");

   RabbitMQ.Client.Connect (C, "amqp://guest:guest@localhost");

   Put_Line ("Connected. Declaring queue...");

   RabbitMQ.Client.Declare_Queue (C, "hello");

   Put_Line ("Queue 'hello' declared.");
   Put_Line ("Waiting for messages (Ctrl+C to stop)...");
   New_Line;

   --  Subscribe blocks until Unsubscribe is called or connection closes
   --  Note: 'Unrestricted_Access is needed for local subprograms
   RabbitMQ.Client.Subscribe
     (C       => C,
      Queue   => "hello",
      Handler => On_Message'Unrestricted_Access);

exception
   when RabbitMQ.Exceptions.Invalid_URL =>
      Put_Line ("ERROR: Invalid AMQP URL.");

   when RabbitMQ.Exceptions.Connection_Error =>
      Put_Line ("ERROR: Failed to connect to RabbitMQ.");
      Put_Line ("Make sure RabbitMQ is running on localhost:5672");

   when RabbitMQ.Exceptions.Channel_Error =>
      Put_Line ("ERROR: Channel operation failed.");
end Client_Receive;
