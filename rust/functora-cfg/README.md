# functora-cfg

A Rust library that merges configuration values from multiple sources into a single typed value. Configuration values are applied in the following order:

- Defaults
- Config file
- Environment variables
- Command-line arguments

All sources are optional. Only the ones you provide will be applied. Here is an [example](https://github.com/functora/functora.github.io/blob/master/rust/functora-cfg/tests/integration.rs) of how to use the library.

<hr>

© 2025 [Functora](https://functora.github.io/). All rights reserved.
