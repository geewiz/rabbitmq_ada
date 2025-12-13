# RabbitMQ Ada

Ada bindings for the [librabbitmq](https://github.com/alanxz/rabbitmq-c) C client library.

## Overview

This library provides Ada bindings for RabbitMQ, allowing Ada programs to communicate with RabbitMQ message brokers using the AMQP 0-9-1 protocol.

The library is structured in two layers:

- **Thin binding** (`RabbitMQ.C_Binding`): Low-level 1:1 mapping to the C API (private)
- **Thick binding** (`RabbitMQ.Connections`, etc.): Ada-idiomatic API with tagged types, exceptions, and automatic resource management
- A **Client package** that provides a simple, high-level interface to RabbitMQ features.

## Features

- Connection management with RAII (automatic cleanup)
- PLAIN authentication
- Exception-based error handling
- Portable across platforms (no platform-specific dependencies)
- TLS support

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
git clone https://github.com/geewiz/rabbitmq_ada.git
cd rabbitmq-ada
alr build
```

## Usage

See the `examples/` directory for usage examples.

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

## License

MIT License - see LICENSE file for details.

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.
