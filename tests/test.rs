#![feature(proc_macro_hygiene)]

use cmd_macro::cmd;

#[test]
fn full_path() {
    let echo = cmd!(/bin/echo foo).expect("could not execute command");
    assert!(echo.status.success());
    let echo = String::from_utf8(echo.stdout).expect("not utf8 string");
    assert_eq!(echo, "foo\n");
}

#[test]
fn piping() {
    let files = cmd!(ls | grep Cargo).expect("could not execute command");
    assert!(files.status.success());
    let files = String::from_utf8(files.stdout).expect("not utf8 string");
    assert_eq!(files, "Cargo.lock\nCargo.toml\n");
}

#[test]
fn interpolation_ident() {
    let cargo = "Cargo";
    let files = cmd!(ls | grep #cargo).expect("could not execute command");
    assert!(files.status.success());
    let files = String::from_utf8(files.stdout).expect("not utf8 string");
    assert_eq!(files, "Cargo.lock\nCargo.toml\n");
}

#[test]
fn interpolation_none() {
    let echo = cmd!(echo ####1###).expect("could not execute command");
    assert!(echo.status.success());
    let echo = String::from_utf8(echo.stdout).expect("not utf8 string");
    assert_eq!(echo, "###1###\n");
}

// TODO: Fix this bug
#[test]
fn acknowledge_braces_bug() {
    let echo = cmd!(echo {a | echo b}).expect("could not execute command");
    assert!(echo.status.success());
    let echo = String::from_utf8(echo.stdout).expect("not utf8 string");
    assert_eq!(echo, "{a}\n");

    let echo_workaround = cmd!(echo #{"{a"} | echo #{"b}"}).expect("could not execute command");
    assert!(echo_workaround.status.success());
    let echo_workaround = String::from_utf8(echo_workaround.stdout).expect("not utf8 string");
    assert_eq!(echo_workaround, "b}\n");
}

#[test]
fn parse_env() {
    let echo = cmd!(echo --foo="$HOME :)").expect("could not execute command");
    let echo = String::from_utf8(echo.stdout).expect("not utf8 string");

    let home = std::env::var("HOME").expect("could not get home var");
    assert_eq!(echo, format!("--foo={} :)\n", home));
}