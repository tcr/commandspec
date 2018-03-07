# command-macro

Simple Rust macro for building `std::process::Command` objects. Uses macro_rules! and works on stable.

```
[dependencies]
command_macro = "0.1.0"
```

Then:

```
#[macro_use]
extern crate command_macro;

use command_macro::ExecuteCommand; // .execute() trait
use std::process::Command;

command!(
    cd: "path/location",
    env: RUST_LOG="full",
    env: RUST_BACKTRACE=1,
    "cargo run {release_flag} --bin {bin_name} -- {args}",
    release_flag=Some("--release"),
    bin_name="binary",
    args=vec!["arg1", "arg2"],
).execute(); // Run and exit out if error code != 0
```

## License

MIT or Apache-2.0, at your option.