extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;
use quote::quote;
use syn::{parse_macro_input, Expr};

#[proc_macro_hack]
pub fn cmd(input: TokenStream) -> TokenStream {
    // input
    TokenStream::from(quote! {
        || -> std::io::Result<std::process::Output> {
            let x0 = std::process::Command::new("ls")
                .stdout(std::process::Stdio::piped())
                .spawn()?;
            let x1 = std::process::Command::new("grep")
                .arg("Cargo")
                .stdin(x0.stdout.unwrap())
                .output()?;
            Ok(x1)
        }()
    })
}