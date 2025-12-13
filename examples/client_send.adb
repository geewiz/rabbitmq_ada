--  client_send.adb - Simple publisher using RabbitMQ.Client high-level API
--
--  Usage: Run this program to publish "Hello World" messages to the
--  "hello" queue using the simplified Client API.

with Ada.Text_IO;

with RabbitMQ.Client;
with RabbitMQ.Exceptions;

procedure Client_Send is
   use Ada.Text_IO;

   C       : RabbitMQ.Client.Connection;
   Counter : Natural := 0;
begin
   Put_Line ("Connecting to RabbitMQ...");

   RabbitMQ.Client.Connect (C, "amqp://guest:guest@localhost");

   Put_Line ("Connected. Declaring queue...");

   RabbitMQ.Client.Declare_Queue (C, "hello");

   Put_Line ("Queue 'hello' declared.");
   Put_Line ("Publishing messages (Ctrl+C to stop)...");
   New_Line;

   loop
      Counter := Counter + 1;

      declare
         Body_Text : constant String :=
           "Hello World #" & Natural'Image (Counter);
      begin
         RabbitMQ.Client.Publish
           (C       => C,
            Queue   => "hello",
            Message => Body_Text);

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
end Client_Send;
