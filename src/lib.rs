use proc_macro_hack::proc_macro_hack;

/// Run a series of commands
#[proc_macro_hack]
pub use cmd_derive::cmd;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let files = cmd!(ls | grep "Cargo").expect("could not execute command");
        assert!(files.status.success());
        let files = String::from_utf8(files.stdout).expect("not utf8 string");
        assert_eq!(files, "Cargo.lock\nCargo.toml\n")
    }
}