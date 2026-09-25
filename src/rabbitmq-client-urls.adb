--  RabbitMQ.Client.URLs - AMQP URL parser implementation

with Ada.Strings.Fixed;
with RabbitMQ.Exceptions;

package body RabbitMQ.Client.URLs is

   use Ada.Strings.Fixed;

   function Parse (URL : String) return Connection_Params is
      Result   : Connection_Params;
      Pos      : Natural;
      Rest     : Unbounded_String;
      Host_Part : Unbounded_String;

      function Find (S : Unbounded_String; C : Character) return Natural is
         Str : constant String := To_String (S);
      begin
         for I in Str'Range loop
            if Str (I) = C then
               return I - Str'First + 1;
            end if;
         end loop;
         return 0;
      end Find;

      function Slice_Before
        (S : Unbounded_String; Idx : Positive) return Unbounded_String is
      begin
         return To_Unbounded_String (Slice (S, 1, Idx - 1));
      end Slice_Before;

      function Slice_After
        (S : Unbounded_String; Idx : Positive) return Unbounded_String is
      begin
         if Idx >= Length (S) then
            return Null_Unbounded_String;
         end if;
         return To_Unbounded_String (Slice (S, Idx + 1, Length (S)));
      end Slice_After;

      function Hex_Value (Digit : Character) return Integer is
      begin
         case Digit is
            when '0' .. '9' =>
               return Character'Pos (Digit) - Character'Pos ('0');
            when 'A' .. 'F' =>
               return Character'Pos (Digit) - Character'Pos ('A') + 10;
            when 'a' .. 'f' =>
               return Character'Pos (Digit) - Character'Pos ('a') + 10;
            when others =>
               return -1;
         end case;
      end Hex_Value;

      function Decode_Vhost (Encoded : Unbounded_String)
        return Unbounded_String
      is
         Source  : constant String := To_String (Encoded);
         Decoded : Unbounded_String := Null_Unbounded_String;
         Index   : Natural := Source'First;
      begin
         while Index <= Source'Last loop
            if Source (Index) = '%' then
               if Index + 2 > Source'Last then
                  raise RabbitMQ.Exceptions.Invalid_URL
                    with "Malformed percent escape in virtual host";
               end if;

               declare
                  High : constant Integer := Hex_Value (Source (Index + 1));
                  Low  : constant Integer := Hex_Value (Source (Index + 2));
               begin
                  if High < 0 or else Low < 0 then
                     raise RabbitMQ.Exceptions.Invalid_URL
                       with "Malformed percent escape in virtual host";
                  end if;

                  if High = 0 and then Low = 0 then
                     raise RabbitMQ.Exceptions.Invalid_URL
                       with "NUL in virtual host";
                  end if;

                  Append (Decoded, Character'Val (High * 16 + Low));
               end;
               Index := Index + 3;
            else
               if Source (Index) = Character'Val (0) then
                  raise RabbitMQ.Exceptions.Invalid_URL
                    with "NUL in virtual host";
               end if;

               Append (Decoded, Source (Index));
               Index := Index + 1;
            end if;
         end loop;

         return Decoded;
      end Decode_Vhost;

   begin
      --  Set defaults
      Result.User := To_Unbounded_String (Default_User);
      Result.Password := To_Unbounded_String (Default_Password);
      Result.Virtual_Host := To_Unbounded_String (Default_Vhost);
      Result.Use_TLS := False;
      Result.Port := Default_Port;

      --  Check for amqp:// or amqps:// scheme
      if URL'Length >= 7 and then URL (URL'First .. URL'First + 6) = "amqp://"
      then
         Result.Use_TLS := False;
         Result.Port := Default_Port;
         Rest := To_Unbounded_String (URL (URL'First + 7 .. URL'Last));
      elsif URL'Length >= 8
        and then URL (URL'First .. URL'First + 7) = "amqps://"
      then
         Result.Use_TLS := True;
         Result.Port := Default_TLS_Port;
         Rest := To_Unbounded_String (URL (URL'First + 8 .. URL'Last));
      else
         raise RabbitMQ.Exceptions.Invalid_URL
           with "URL must start with amqp:// or amqps://";
      end if;

      --  Empty host is invalid
      if Length (Rest) = 0 then
         raise RabbitMQ.Exceptions.Invalid_URL with "Missing host in URL";
      end if;

      --  Check for credentials (user:password@)
      Pos := Find (Rest, '@');
      if Pos > 0 then
         declare
            Creds : constant Unbounded_String := Slice_Before (Rest, Pos);
            Colon : constant Natural := Find (Creds, ':');
         begin
            if Colon > 0 then
               Result.User := Slice_Before (Creds, Colon);
               Result.Password := Slice_After (Creds, Colon);
            else
               Result.User := Creds;
               Result.Password := Null_Unbounded_String;
            end if;
         end;
         Rest := Slice_After (Rest, Pos);
      end if;

      --  Check for virtual host (/vhost)
      Pos := Find (Rest, '/');
      if Pos > 0 then
         Host_Part := Slice_Before (Rest, Pos);
         Result.Virtual_Host := Decode_Vhost (Slice_After (Rest, Pos));
      else
         Host_Part := Rest;
      end if;

      --  Parse host:port
      Pos := Find (Host_Part, ':');
      if Pos > 0 then
         Result.Host := Slice_Before (Host_Part, Pos);
         declare
            Port_Str : constant String :=
              To_String (Slice_After (Host_Part, Pos));
         begin
            Result.Port := Natural'Value (Port_Str);
         exception
            when Constraint_Error =>
               raise RabbitMQ.Exceptions.Invalid_URL
                 with "Invalid port number: " & Port_Str;
         end;
      else
         Result.Host := Host_Part;
      end if;

      --  Validate we have a host
      if Length (Result.Host) = 0 then
         raise RabbitMQ.Exceptions.Invalid_URL with "Missing host in URL";
      end if;

      return Result;
   end Parse;

end RabbitMQ.Client.URLs;
