//! Run shell scripts inline with your rust code
//!
//! # Example
//! ```
//! #![feature(proc_macro_hygiene)]
//! use cmd_macro::cmd;
//!
//! fn main() {
//!     let who = "world";
//!     let output = cmd!(echo hello #who ##who).unwrap();
//!     let output = String::from_utf8(output.stdout).unwrap();
//!     assert_eq!(output, "hello world #who\n");
//!
//!     let s = "seq";
//!     let n = 3;
//!     let output = cmd!(#s #{2 * n}).unwrap();
//!     let output = String::from_utf8(output.stdout).unwrap();
//!     assert_eq!(output, "1\n2\n3\n4\n5\n6\n");
//! }
//! ```

#![forbid(unsafe_code)]

/// Run a series of commands
///
/// # Example
/// ```
/// #![feature(proc_macro_hygiene)]
/// use cmd_macro::cmd;
///
/// fn main() {
///     let who = "world";
///     let output = cmd!(echo hello #who ##who).unwrap();
///     let output = String::from_utf8(output.stdout).unwrap();
///     assert_eq!(output, "hello world #who\n");
///
///     let s = "seq";
///     let n = 3;
///     let output = cmd!(#s #{2 * n}).unwrap();
///     let output = String::from_utf8(output.stdout).unwrap();
///     assert_eq!(output, "1\n2\n3\n4\n5\n6\n");
/// }
/// ```
pub use cmd_derive::cmd;

pub mod env;

use thiserror::Error;

#[derive(Debug, Error)]
pub enum ShellError {
    #[error("expanding args")]
    ExpandError(#[from] std::env::VarError),
    #[error("parsing args")]
    ParseError(#[from] env::ParseError),
    #[error("executing command")]
    ExectutionError(#[from] std::io::Error),
}

pub type Result<T> = std::result::Result<T, ShellError>;