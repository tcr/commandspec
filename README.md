# commandspec

Simple Rust macro for building `std::process::Command` objects. Uses macro_rules! and works on stable.

```toml
[dependencies]
commandspec = "0.2.0"
```

Then:

```rust
#[macro_use]
extern crate commandspec;

use commandspec::CommandSpec; // .execute() method on Command
use std::process::Command;

command!(
    "
    cd path/location
    export RUST_LOG=full
    export RUST_BACKTRACE=1
    cargo run {release_flag} --bin {bin_name} -- {args}
    ",
    release_flag=Some("--release"),
    bin_name="binary",
    args=vec!["arg1", "arg2"],
)?.execute()?; // () on success (error code 0), CommandError for all else
```

### Features:

* format-like invocation makes it easy to interpolate variables, with automatic quoting
* Equivalent syntax to shell when prototyping
* Works on stable Rust.

## License

MIT or Apache-2.0, at your option.