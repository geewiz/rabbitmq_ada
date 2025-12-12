--  Test program for RabbitMQ connection and channel functionality

with Ada.Text_IO;
with Ada.Exceptions;
with RabbitMQ.Connections;
with RabbitMQ.Channels;
with RabbitMQ.Exceptions;

procedure Test_Connection is
   use Ada.Text_IO;

   Conn : RabbitMQ.Connections.Connection;
   Ch   : RabbitMQ.Channels.Channel;
begin
   Put_Line ("RabbitMQ Connection and Channel Test");
   Put_Line ("====================================");
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
      end if;

      Put_Line ("   Closing channel...");
      RabbitMQ.Channels.Close (Ch);
      Put_Line ("   Channel closed.");

      New_Line;

      --  Test multiple channels
      Put_Line ("3. Testing Multiple Channels");
      declare
         Ch1, Ch2, Ch3 : RabbitMQ.Channels.Channel;
      begin
         Put_Line ("   Opening channels 1, 2, and 3...");
         RabbitMQ.Channels.Open (Ch1, Conn, Number => 1);
         RabbitMQ.Channels.Open (Ch2, Conn, Number => 2);
         RabbitMQ.Channels.Open (Ch3, Conn, Number => 3);

         if RabbitMQ.Channels.Is_Open (Ch1) and
            RabbitMQ.Channels.Is_Open (Ch2) and
            RabbitMQ.Channels.Is_Open (Ch3)
         then
            Put_Line ("   SUCCESS: All three channels opened!");
         else
            Put_Line ("   FAILED: Not all channels are open");
         end if;

         Put_Line ("   Closing channels...");
         RabbitMQ.Channels.Close (Ch1);
         RabbitMQ.Channels.Close (Ch2);
         RabbitMQ.Channels.Close (Ch3);
         Put_Line ("   All channels closed.");
      end;

      New_Line;

      --  Clean up
      Put_Line ("4. Cleanup");
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
