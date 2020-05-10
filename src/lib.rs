//! Run shell scripts inline with your rust code
//!
//! # Example
//! ```
//! #![feature(proc_macro_hygiene)]
//! use cmd::cmd;
//!
//! fn main() {
//!     let who = "world";
//!     let output = cmd!(echo hello #who ##who);
//!     assert!(String::from(output.stdout), Ok("hello world #who\n"));
//!
//!     let s = "seq"
//!     let n = 3;
//!     let output = cmd!(#s #{2 * n});
//!     assert!(String::from(output.stdout), Ok("1\n2\n3\n4\n5\n6\n"));
//! }
//! ```

/// Run a series of commands
///
/// # Example
/// ```
/// #![feature(proc_macro_hygiene)]
/// use cmd::cmd;
///
/// fn main() {
///     let who = "world";
///     let output = cmd!(echo hello #who ##who);
///     assert!(String::from(output.stdout), Ok("hello world #who\n"));
///
///     let s = "seq"
///     let n = 3;
///     let output = cmd!(#s #{2 * n});
///     assert!(String::from(output.stdout), Ok("1\n2\n3\n4\n5\n6\n"));
/// }
/// ```
pub use cmd_derive::cmd;