extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as Stream, Span};
use proc_macro_hack::proc_macro_hack;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Expr, Ident};

#[proc_macro_hack]
pub fn cmd(input: TokenStream) -> TokenStream {
    // let root = parse_macro_input!(input as Cmd);
    
    // input = `ls | grep "Cargo"`
    let root = Cmd{
        env: None,
        commands: vec![
            Command {span: Span::call_site(), name: "ls".to_string(), args: vec![]},
            Command {span: Span::call_site(), name: "grep".to_string(), args: vec!["Cargo".to_string()]},
        ],
        stdin: None,
    };
    TokenStream::from(quote! {#root})
}

struct Cmd {
    env: Option<Expr>,
    commands: Vec<Command>,
    stdin: Option<Expr>,
}

struct Command {
    span: Span,
    name: String,
    args: Vec<String>
}

// impl Parse for Cmd {

// }

impl ToTokens for Cmd {
    fn to_tokens(&self, tokens: &mut Stream) {
        if self.commands.len() == 0 {
            tokens.extend(quote!{
                Err(
                    std::io::Error::new(
                        std::io::ErrorKind::Other,
                        "no command input",
                    )
                )
            });
            return
        }

        let mut prev: Option<Ident> = None;
        let cmds: Vec<Stream> = self.commands.iter().zip(0..).map(|(cmd, i)| {
            
            let ident = Ident::new(&format!("x{}", i), cmd.span);

            let command = cmd.to_tokens(ident.clone(), prev.clone(), i == self.commands.len() - 1);

            prev = Some(ident);

            command
        }).collect();

        let prev = prev.unwrap();

        tokens.extend(quote!{
            || -> std::io::Result<std::process::Output> {
                #(
                    #cmds;
                )*
                Ok(#prev)
            }()
        })
    }
}

impl Command {
    fn to_tokens(&self, ident: Ident, prev: Option<Ident>, last: bool) -> Stream {
        let name = self.name.clone();
        let command = self.args.iter().fold(
            quote!{ std::process::Command::new(#name) },
            |command, arg| quote!{ #command.arg(#arg) },
        );
        let command = match prev {
            Some(p) => quote!{ #command.stdin(#p.stdout.unwrap()) },
            None => command,
        };

        if last {
            quote!{ let #ident = #command.output()? }
        } else {
            quote!{ let #ident = #command.stdout(std::process::Stdio::piped()).spawn()? }
        }
    }
}