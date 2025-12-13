--  hw_send.adb - Publishes "Hello World" messages with a counter
--
--  Usage: Run this program to continuously publish messages to the
--  "hello" fanout exchange.

with Ada.Text_IO;

with RabbitMQ.Connections;
with RabbitMQ.Channels;
with RabbitMQ.Exchanges;
with RabbitMQ.Publishing;
with RabbitMQ.Messages;
with RabbitMQ.Exceptions;

procedure HW_Send is
   use Ada.Text_IO;

   Conn    : RabbitMQ.Connections.Connection;
   Ch      : RabbitMQ.Channels.Channel;
   Counter : Natural := 0;

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
   Put_Line ("Publishing messages (Ctrl+C to stop)...");
   New_Line;

   loop
      Counter := Counter + 1;

      declare
         Body_Text : constant String :=
           "Hello World #" & Natural'Image (Counter);
      begin
         RabbitMQ.Publishing.Publish
           (Ch          => Ch,
            Exchange    => Exchange_Name,
            Routing_Key => "",
            Data        => Body_Text);

         Put_Line ("[x] Sent: " & Body_Text);
      end;

      delay 2.0;
   end loop;

exception
   when RabbitMQ.Exceptions.Connection_Error =>
      Put_Line ("ERROR: Failed to connect to RabbitMQ.");
      Put_Line ("Make sure RabbitMQ is running on localhost:5672");

   when RabbitMQ.Exceptions.Channel_Error =>
      Put_Line ("ERROR: Channel operation failed.");
end HW_Send;
