# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.2.0] - 2026-09-30

### Added

- Add OpenSUSE, CentOS, and RHEL distribution integration and document installing `librabbitmq-devel` on these platforms ([#3](https://github.com/geewiz/rabbitmq_ada/pull/3)).

### Changed

- Rename the compiled Ada library to `rabbitmq_ada` to avoid a case-insensitive Windows filename collision with the `librabbitmq` dependency ([#3](https://github.com/geewiz/rabbitmq_ada/pull/3)).

## [0.1.1] - 2026-09-25

### Fixed

- Parse AMQP URI virtual hosts without including the path separator, preserve an explicitly empty virtual host, and decode percent-encoded names such as `/%2F` ([#1](https://github.com/geewiz/rabbitmq_ada/issues/1)).

[Unreleased]: https://github.com/geewiz/rabbitmq_ada/compare/v0.2.0...HEAD
[0.2.0]: https://github.com/geewiz/rabbitmq_ada/compare/v0.1.1...v0.2.0
[0.1.1]: https://github.com/geewiz/rabbitmq_ada/compare/8884fd726f01b797952703a1cfabfad866318d68...3f587173e29138ea1598feb8eac83721474f0d96
