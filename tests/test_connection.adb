--  Test program for RabbitMQ connection functionality

with Ada.Text_IO;
with Ada.Exceptions;
with RabbitMQ.Connections;
with RabbitMQ.Exceptions;

procedure Test_Connection is
   use Ada.Text_IO;

   Conn : RabbitMQ.Connections.Connection;
begin
   Put_Line ("RabbitMQ Connection Test");
   Put_Line ("========================");
   New_Line;

   Put_Line ("Connecting to localhost:5672 as guest...");

   begin
      RabbitMQ.Connections.Connect
        (Conn         => Conn,
         Host         => "localhost",
         Port         => 5672,
         User         => "guest",
         Password     => "guest",
         Virtual_Host => "/");

      if RabbitMQ.Connections.Is_Open (Conn) then
         Put_Line ("SUCCESS: Connected to RabbitMQ broker!");
      else
         Put_Line ("FAILED: Connection reports not open");
      end if;

      Put_Line ("Closing connection...");
      RabbitMQ.Connections.Close (Conn);
      Put_Line ("Connection closed.");

   exception
      when E : RabbitMQ.Exceptions.Connection_Error =>
         Put_Line ("Connection error: " &
                   Ada.Exceptions.Exception_Message (E));
         Put_Line ("Make sure RabbitMQ is running on localhost:5672");

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
