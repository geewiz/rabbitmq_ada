--  RabbitMQ Ada bindings - Connection management
--  Provides Ada-idiomatic connection handling with RAII

private with Ada.Finalization;
with RabbitMQ.C_Binding;

package RabbitMQ.Connections is

   --  Connection to a RabbitMQ broker
   --  Automatically closes when it goes out of scope
   type Connection is tagged limited private;

   --  Default connection parameters
   Default_Port : constant := 5672;
   Default_User : constant String := "guest";
   Default_Password : constant String := "guest";
   Default_Virtual_Host : constant String := "/";
   Default_Channel_Max : constant := 0;      --  No limit (use server default)
   Default_Frame_Max : constant := 131072;   --  128KB
   Default_Heartbeat : constant := 0;        --  No heartbeat

   --  Create a new connection to a RabbitMQ broker
   --  Raises Connection_Error on failure
   procedure Connect
     (Conn         : out Connection;
      Host         : String;
      Port         : Natural := Default_Port;
      User         : String := Default_User;
      Password     : String := Default_Password;
      Virtual_Host : String := Default_Virtual_Host;
      Channel_Max  : Natural := Default_Channel_Max;
      Frame_Max    : Natural := Default_Frame_Max;
      Heartbeat    : Natural := Default_Heartbeat);

   --  Check if connection is open
   function Is_Open (Conn : Connection) return Boolean;

   --  Close the connection explicitly
   --  Safe to call multiple times
   procedure Close (Conn : in out Connection);

   --  Get the underlying connection state for use with thin binding
   --  For advanced users who need direct access to C API
   function Get_State
     (Conn : Connection) return C_Binding.amqp_connection_state_t;

private

   type Connection is new Ada.Finalization.Limited_Controlled with record
      State  : C_Binding.amqp_connection_state_t := null;
      Socket : access C_Binding.amqp_socket_t_u := null;
      Open   : Boolean := False;
   end record;

   overriding procedure Finalize (Conn : in out Connection);

end RabbitMQ.Connections;
