macro_rules! cmd {
    () => {};
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        let files = cmd!(ls -l | grep "Cargo").expect("could not execute command");
        assert_eq!(files.status, 0);
        let files = String::from_utf8(files.stdout).expect("not utf8 string");
        assert_eq!(files, "Cargo.lock\nCargo.toml")
    }
}