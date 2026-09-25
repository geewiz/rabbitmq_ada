--  Copyright (C) 2026 Jochen Lillich
--  SPDX-License-Identifier: MIT

with Ada.Command_Line;
with Ada.Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with RabbitMQ.Client.URLs;
with RabbitMQ.Exceptions;

procedure Test_Client_URLs is
   use Ada.Text_IO;
   package SU renames Ada.Strings.Unbounded;
   package URLs renames RabbitMQ.Client.URLs;

   Failures : Natural := 0;

   procedure Report_Failure (Name : String; Detail : String);

   procedure Report_Failure (Name : String; Detail : String) is
   begin
      Put_Line ("FAIL: " & Name & " (" & Detail & ")");
      Failures := Failures + 1;
   end Report_Failure;

   procedure Check_Parse
     (Name          : String;
      URL           : String;
      Virtual_Host  : String;
      Host          : String := "broker";
      Port          : Natural := URLs.Default_Port;
      User          : String := URLs.Default_User;
      Password      : String := URLs.Default_Password;
      Use_TLS       : Boolean := False);

   procedure Check_Parse
     (Name          : String;
      URL           : String;
      Virtual_Host  : String;
      Host          : String := "broker";
      Port          : Natural := URLs.Default_Port;
      User          : String := URLs.Default_User;
      Password      : String := URLs.Default_Password;
      Use_TLS       : Boolean := False)
   is
   begin
      declare
         Parsed : constant URLs.Connection_Params := URLs.Parse (URL);
      begin
         if SU.To_String (Parsed.Virtual_Host) /= Virtual_Host then
            Report_Failure
              (Name,
               "virtual host expected '" & Virtual_Host & "', got '" &
                 SU.To_String (Parsed.Virtual_Host) & "'");
         end if;
         if SU.To_String (Parsed.Host) /= Host then
            Report_Failure
              (Name,
               "host expected '" & Host & "', got '" &
                 SU.To_String (Parsed.Host) & "'");
         end if;
         if Parsed.Port /= Port then
            Report_Failure
              (Name,
               "port expected" & Port'Image & ", got" & Parsed.Port'Image);
         end if;
         if SU.To_String (Parsed.User) /= User then
            Report_Failure (Name, "user did not match expected value");
         end if;
         if SU.To_String (Parsed.Password) /= Password then
            Report_Failure (Name, "password did not match expected value");
         end if;
         if Parsed.Use_TLS /= Use_TLS then
            Report_Failure (Name, "TLS setting did not match expected value");
         end if;
         if SU.To_String (Parsed.Virtual_Host) = Virtual_Host
           and then SU.To_String (Parsed.Host) = Host
           and then Parsed.Port = Port
           and then SU.To_String (Parsed.User) = User
           and then SU.To_String (Parsed.Password) = Password
           and then Parsed.Use_TLS = Use_TLS
         then
            Put_Line ("PASS: " & Name);
         end if;
      end;
   exception
      when E : others =>
         Report_Failure
           (Name, "raised " & Ada.Exceptions.Exception_Name (E));
   end Check_Parse;

   procedure Check_Invalid_URL (Name : String; URL : String);

   procedure Check_Invalid_URL (Name : String; URL : String) is
      User_Secret     : constant String := "parser-test-user";
      Password_Secret : constant String := "parser-test-password";
   begin
      declare
         Parsed : constant URLs.Connection_Params := URLs.Parse (URL);
         pragma Unreferenced (Parsed);
      begin
         Report_Failure (Name, "expected Invalid_URL");
      end;
   exception
      when E : RabbitMQ.Exceptions.Invalid_URL =>
         declare
            Message : constant String := Ada.Exceptions.Exception_Message (E);
         begin
            if Ada.Strings.Fixed.Index (Message, URL) > 0
              or else Ada.Strings.Fixed.Index (Message, User_Secret) > 0
             or else Ada.Strings.Fixed.Index (Message, Password_Secret) > 0
            then
               Report_Failure
                 (Name, "exception message exposed URL credentials");
            else
               Put_Line ("PASS: " & Name);
            end if;
         end;
      when E : others =>
         Report_Failure
            (Name, "expected Invalid_URL, got " &
               Ada.Exceptions.Exception_Name (E));
   end Check_Invalid_URL;

   procedure Check_Scheme
     (Prefix : String; Expected_TLS : Boolean; Expected_Port : Natural);

   procedure Check_Scheme
     (Prefix : String; Expected_TLS : Boolean; Expected_Port : Natural)
   is
   begin
      Check_Parse
        ("named vhost, no credentials, " & Prefix,
         Prefix & "broker/lora", "lora",
         Port => Expected_Port, Use_TLS => Expected_TLS);
      Check_Parse
        ("named vhost, credentials, " & Prefix,
         Prefix & "alice:secret@broker/lora", "lora",
         Port => Expected_Port, User => "alice", Password => "secret",
         Use_TLS => Expected_TLS);
      Check_Parse
        ("empty vhost, no credentials, " & Prefix,
         Prefix & "broker/", "",
         Port => Expected_Port, Use_TLS => Expected_TLS);
      Check_Parse
        ("empty vhost, credentials, " & Prefix,
         Prefix & "alice:secret@broker/", "",
         Port => Expected_Port, User => "alice", Password => "secret",
         Use_TLS => Expected_TLS);
      Check_Parse
        ("default vhost, no credentials, " & Prefix,
         Prefix & "broker", "/",
         Port => Expected_Port, Use_TLS => Expected_TLS);
      Check_Parse
        ("default vhost, credentials, " & Prefix,
         Prefix & "alice:secret@broker", "/",
         Port => Expected_Port, User => "alice", Password => "secret",
         Use_TLS => Expected_TLS);
      Check_Parse
        ("explicit port and vhost, " & Prefix,
         Prefix & "alice:secret@broker:4321/lora", "lora",
         Port => 4321, User => "alice", Password => "secret",
         Use_TLS => Expected_TLS);
   end Check_Scheme;

begin
   Check_Scheme ("amqp://", False, URLs.Default_Port);
   Check_Scheme ("amqps://", True, URLs.Default_TLS_Port);

   Check_Parse ("uppercase encoded slash", "amqp://broker/%2F", "/");
   Check_Parse ("lowercase encoded slash", "amqp://broker/%2f", "/");
   Check_Parse ("encoded character", "amqp://broker/l%6Fra", "lora");
   Check_Parse ("encoded path separator", "amqp://broker/a%2Fb", "a/b");
   Check_Parse ("single-pass decoding", "amqp://broker/%252F", "%2F");
   Check_Parse ("literal plus", "amqp://broker/a+b", "a+b");
   Check_Parse ("encoded plus", "amqp://broker/a%2Bb", "a+b");

   Check_Invalid_URL
     ("trailing percent",
      "amqp://parser-test-user:parser-test-password@broker/%");
   Check_Invalid_URL
     ("truncated escape",
      "amqp://parser-test-user:parser-test-password@broker/%2");
   Check_Invalid_URL
     ("invalid escape",
      "amqp://parser-test-user:parser-test-password@broker/%GG");
   Check_Invalid_URL
     ("invalid escape digit",
      "amqp://parser-test-user:parser-test-password@broker/%2G");
   Check_Invalid_URL
     ("encoded NUL",
      "amqp://parser-test-user:parser-test-password@broker/%00");
   Check_Invalid_URL
     ("literal NUL",
      "amqp://parser-test-user:parser-test-password@broker/literal" &
        Character'Val (0));
   Check_Invalid_URL
     ("unsupported scheme",
      "http://parser-test-user:parser-test-password@broker/lora");
   Check_Invalid_URL ("missing host", "amqp:///lora");
   Check_Invalid_URL
     ("missing host with credentials",
      "amqp://parser-test-user:parser-test-password@/lora");
   Check_Invalid_URL
     ("invalid port before path",
      "amqp://parser-test-user:parser-test-password@broker:not-a-port/lora");

   if Failures > 0 then
      Put_Line ("Client URL parser tests failed:" & Failures'Image);
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      Put_Line ("All client URL parser tests passed.");
   end if;
exception
   when E : others =>
      Put_Line
         ("FAIL: test harness exception (" &
            Ada.Exceptions.Exception_Name (E) & ")");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
end Test_Client_URLs;
