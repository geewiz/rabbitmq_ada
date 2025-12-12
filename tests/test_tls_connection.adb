--  Test program for RabbitMQ TLS connections

with Ada.Text_IO;
with Ada.Exceptions;
with RabbitMQ.Connections;
with RabbitMQ.Channels;
with RabbitMQ.Queues;
with RabbitMQ.Publishing;
with RabbitMQ.Consuming;
with RabbitMQ.Exceptions;

procedure Test_TLS_Connection is
   use Ada.Text_IO;

   --  Default CA certificate path (can be changed for your setup)
   CA_Cert_Path : constant String := "/tmp/rabbitmq-certs/ca_certificate.pem";

   Conn : RabbitMQ.Connections.Connection;
   Ch   : RabbitMQ.Channels.Channel;

   TLS_Opts : constant RabbitMQ.Connections.TLS_Options :=
     RabbitMQ.Connections.TLS_With_CA_Cert (CA_Cert_Path);

begin
   Put_Line ("RabbitMQ Ada Bindings - TLS Connection Test");
   Put_Line ("============================================");
   New_Line;

   Put_Line ("Using CA certificate: " & CA_Cert_Path);
   New_Line;

   begin
      --  Test TLS connection
      Put_Line ("1. Testing TLS Connection");
      Put_Line ("   Connecting to localhost:5671 with TLS...");

      RabbitMQ.Connections.Connect_TLS
        (Conn         => Conn,
         Host         => "localhost",
         Port         => 5671,
         User         => "guest",
         Password     => "guest",
         Virtual_Host => "/",
         TLS          => TLS_Opts);

      if RabbitMQ.Connections.Is_Open (Conn) then
         Put_Line ("   SUCCESS: Connected via TLS!");
      else
         Put_Line ("   FAILED: Connection reports not open");
         return;
      end if;

      New_Line;

      --  Test channel over TLS connection
      Put_Line ("2. Testing Channel over TLS");
      Put_Line ("   Opening channel 1...");

      RabbitMQ.Channels.Open (Ch, Conn, Number => 1);

      if RabbitMQ.Channels.Is_Open (Ch) then
         Put_Line ("   SUCCESS: Channel opened over TLS connection!");
      else
         Put_Line ("   FAILED: Channel reports not open");
         return;
      end if;

      New_Line;

      --  Test queue operations over TLS
      Put_Line ("3. Testing Queue Operations over TLS");
      declare
         Info : constant RabbitMQ.Queues.Queue_Info :=
           RabbitMQ.Queues.Declare_Queue
             (Ch          => Ch,
              Name        => "tls-test-queue",
              Durable     => False,
              Exclusive   => False,
              Auto_Delete => True);
         pragma Unreferenced (Info);
      begin
         Put_Line ("   SUCCESS: Queue declared over TLS");
      end;

      --  Test publish/consume over TLS
      Put_Line ("   Publishing message over TLS...");
      RabbitMQ.Publishing.Publish_To_Queue
        (Ch    => Ch,
         Queue => "tls-test-queue",
         Data  => "Hello over TLS!");
      Put_Line ("   SUCCESS: Message published");

      Put_Line ("   Consuming message over TLS...");
      declare
         Delivery : RabbitMQ.Consuming.Delivery;
         Got_Msg  : Boolean;
      begin
         Got_Msg := RabbitMQ.Consuming.Get_Message
           (Ch     => Ch,
            Queue  => "tls-test-queue",
            Msg    => Delivery,
            No_Ack => True);

         if Got_Msg then
            Put_Line ("   SUCCESS: Got message over TLS");
         else
            Put_Line ("   WARNING: No message received");
         end if;
      end;

      --  Cleanup
      declare
         Deleted : constant Natural :=
           RabbitMQ.Queues.Delete (Ch, "tls-test-queue");
         pragma Unreferenced (Deleted);
      begin
         null;
      end;

      New_Line;

      --  Clean up
      Put_Line ("4. Cleanup");
      Put_Line ("   Closing channel...");
      RabbitMQ.Channels.Close (Ch);
      Put_Line ("   Channel closed.");
      Put_Line ("   Closing TLS connection...");
      RabbitMQ.Connections.Close (Conn);
      Put_Line ("   TLS Connection closed.");

   exception
      when E : RabbitMQ.Exceptions.SSL_Error =>
         Put_Line ("SSL error: " & Ada.Exceptions.Exception_Message (E));
         Put_Line ("Make sure RabbitMQ is configured for TLS on port 5671");
         Put_Line ("and the CA certificate path is correct.");

      when E : RabbitMQ.Exceptions.Connection_Error =>
         Put_Line ("Connection error: " &
                   Ada.Exceptions.Exception_Message (E));
         Put_Line ("Make sure RabbitMQ is running with TLS on localhost:5671");

      when E : RabbitMQ.Exceptions.Channel_Error =>
         Put_Line ("Channel error: " &
                   Ada.Exceptions.Exception_Message (E));

      when E : RabbitMQ.Exceptions.Socket_Error =>
         Put_Line ("Socket error: " &
                   Ada.Exceptions.Exception_Message (E));
         Put_Line ("Make sure RabbitMQ is running with TLS on localhost:5671");

      when E : others =>
         Put_Line ("Unexpected error: " &
                   Ada.Exceptions.Exception_Information (E));
   end;

   New_Line;
   Put_Line ("TLS Test complete.");
end Test_TLS_Connection;
