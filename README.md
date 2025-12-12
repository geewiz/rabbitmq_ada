# RabbitMQ Ada

Ada bindings for the [librabbitmq](https://github.com/alanxz/rabbitmq-c) C client library.

## Overview

This library provides Ada bindings for RabbitMQ, allowing Ada programs to communicate with RabbitMQ message brokers using the AMQP 0-9-1 protocol.

The library is structured in two layers:

- **Thin binding** (`RabbitMQ.C_Binding`): Low-level 1:1 mapping to the C API (private)
- **Thick binding** (`RabbitMQ.Connections`, etc.): Ada-idiomatic API with tagged types, exceptions, and automatic resource management

## Features

- Connection management with RAII (automatic cleanup)
- PLAIN authentication
- Exception-based error handling
- Portable across platforms (no platform-specific dependencies)

## Requirements

- GNAT Ada compiler (tested with GNAT 13)
- librabbitmq development package
  - Ubuntu/Debian: `sudo apt install librabbitmq-dev`
  - Fedora: `sudo dnf install librabbitmq-devel`

## Installation

Using [Alire](https://alire.ada.dev/):

```bash
alr with rabbitmq
```

Or clone and build manually:

```bash
git clone https://github.com/geewiz/rabbitmq-ada.git
cd rabbitmq-ada
alr build
```

## Usage

```ada
with Ada.Text_IO;
with RabbitMQ.Connections;
with RabbitMQ.Exceptions;

procedure Example is
   Conn : RabbitMQ.Connections.Connection;
begin
   --  Connect to RabbitMQ broker
   RabbitMQ.Connections.Connect
     (Conn         => Conn,
      Host         => "localhost",
      Port         => 5672,
      User         => "guest",
      Password     => "guest",
      Virtual_Host => "/");

   if RabbitMQ.Connections.Is_Open (Conn) then
      Ada.Text_IO.Put_Line ("Connected!");
   end if;

   --  Connection is automatically closed when Conn goes out of scope
exception
   when E : RabbitMQ.Exceptions.Connection_Error =>
      Ada.Text_IO.Put_Line ("Failed to connect");
end Example;
```

## Testing

### Prerequisites

- Docker (for running RabbitMQ)
- Ubuntu 24.04 development environment (or compatible)

### Running Tests

1. Start a RabbitMQ instance:

   ```bash
   docker run -d --name rabbitmq-test -p 5672:5672 rabbitmq:3-management
   ```

2. Wait a few seconds for RabbitMQ to start, then build and run the tests:

   ```bash
   gprbuild -P tests/tests.gpr
   bin/test_connection
   ```

3. Clean up when done:

   ```bash
   docker stop rabbitmq-test && docker rm rabbitmq-test
   ```

### Expected Output

```
RabbitMQ Connection Test
========================

Connecting to localhost:5672 as guest...
SUCCESS: Connected to RabbitMQ broker!
Closing connection...
Connection closed.

Test complete.
```

## API Reference

### RabbitMQ.Connections

| Subprogram | Description |
|------------|-------------|
| `Connect` | Establish a connection to a RabbitMQ broker |
| `Is_Open` | Check if the connection is open |
| `Close` | Explicitly close the connection |
| `Get_State` | Get the underlying C connection state (advanced use) |

### RabbitMQ.Exceptions

| Exception | Description |
|-----------|-------------|
| `RabbitMQ_Error` | Base exception for all RabbitMQ errors |
| `Connection_Error` | Failed to establish connection |
| `Connection_Closed` | Connection was closed unexpectedly |
| `Socket_Error` | TCP socket error |
| `Socket_Closed` | Socket was closed |
| `Protocol_Error` | AMQP protocol error |
| `Timeout_Error` | Operation timed out |
| `Heartbeat_Timeout` | Heartbeat timeout |
| `SSL_Error` | SSL/TLS error |
| `No_Memory` | Memory allocation failed |
| `Invalid_Parameter` | Invalid parameter passed |
| `Table_Too_Big` | AMQP table exceeds size limit |
| `Hostname_Resolution_Failed` | DNS lookup failed |
| `Incompatible_Protocol_Version` | Server uses incompatible AMQP version |

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.
