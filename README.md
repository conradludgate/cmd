# cmd
Macro to help with use the `std::process::Command` feature of Rust. (only works on nightly)

## Usage
```rust
#![feature(proc_macro_hygiene)]

use cmd_macro::cmd;

fn main() {
    let who = "world";
    let output = cmd!(echo hello #who ##who).unwrap();
    let output = String::from_utf8(output.stdout).unwrap();
    assert_eq!(output, "hello world #who\n");

    let s = "seq";
    let n = 3;
    let output = cmd!(#s #{2 * n}).unwrap();
    let output = String::from_utf8(output.stdout).unwrap();
    assert_eq!(output, "1\n2\n3\n4\n5\n6\n");
}
```

This will expand to
```rust
fn main() {
    let who = "world";
    let output = (|| -> std::io::Result<std::process::Output> {
        let x0 = std::process::Command::new("echo")
            .arg("hello")
            .arg(format!("{}", who))
            .arg("#who")
            .output()?;
        Ok(x0)
    })()
    .unwrap();
    let output = String::from_utf8(output.stdout).unwrap();
    assert_eq!(output, "hello world #who\n");
    
    let s = "seq";
    let n = 3;
    let output = (|| -> std::io::Result<std::process::Output> {
        let x0 = std::process::Command::new(format!("{}", s))
        .arg(format!("{}", 2 * n))
        .output()?;
        Ok(x0)
    })()
    .unwrap();
    let output = String::from_utf8(output.stdout).unwrap();
    assert_eq!(output, "1\n2\n3\n4\n5\n6\n");
}
```

Also supports limited piping

```rust
let cargo_files = cmd!(ls | grep "Cargo").unwrap();
let cargo_files = String::from_utf8(cargo_files.stdout).unwrap();
assert_eq!(cargo_files, "Cargo.lock\nCargo.toml\n");
```

which is parsed into

```rust
let cargo_files = (|| -> std::io::Result<std::process::Output> {
    let x0 = std::process::Command::new("ls")
        .stdout(std::process::Stdio::piped())
        .spawn()?;
    let x1 = std::process::Command::new("ls")
        .stdin(x0.stdout.unwrap())
        .output()?;
    Ok(x1)
})()
.unwrap();
let cargo_files = String::from_utf8(cargo_files.stdout).unwrap();
assert_eq!(cargo_files, "Cargo.lock\nCargo.toml\n");
```