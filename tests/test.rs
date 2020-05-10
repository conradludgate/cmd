#![feature(proc_macro_hygiene)]

use cmd::cmd;

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
    let files = cmd!(echo ####1###).expect("could not execute command");
    assert!(files.status.success());
    let files = String::from_utf8(files.stdout).expect("not utf8 string");
    assert_eq!(files, "###1###\n");
}