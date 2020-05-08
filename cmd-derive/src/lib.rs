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
        envs: None,
        commands: vec![
            Command {span: Span::call_site(), name: "ls".to_string(), args: vec![]},
            Command {span: Span::call_site(), name: "grep".to_string(), args: vec!["Cargo".to_string()]},
        ],
        stdin: None,
    };
    TokenStream::from(root.into_token_stream())
}

struct Cmd {
    envs: Option<Expr>,
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
        let mut stdin = self.stdin.clone().map(|stdin| quote!{ #stdin });
        let cmds: Vec<Stream> = self.commands.iter().zip(0..).map(|(cmd, i)| {
            
            let ident = Ident::new(&format!("x{}", i), cmd.span);

            let command = cmd.to_tokens(self.envs.clone(), ident.clone(), stdin.clone(), i == self.commands.len() - 1);

            prev = Some(ident);
            stdin = prev.clone().map(|p| quote!{
                #p.stdout.unwrap()
            });

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
    fn to_tokens(&self, envs: Option<Expr>, ident: Ident, stdin: Option<Stream>, last: bool) -> Stream {
        let name = self.name.clone();
        let command = self.args.iter().fold(
            quote!{ std::process::Command::new(#name) }, // Create the command
            |command, arg| quote!{ #command.arg(#arg) }, // and add all the args
        );

        // If any envs exist, add them to the command
        // Envs must be of type IntoIterator(Item=(K, V)) where K, V: AsRef<OsStr>,
        // eg HashMap<String,String>
        let command = match envs {
            Some(envs) => quote!{ #command.envs(#envs) },
            None => command,
        };

        // If a previous command exists, add it's stdin to the stdin
        // TODO: expand on this in future to support stderr => stdin
        let command = match stdin {
            Some(stdin) => quote!{ #command.stdin(#stdin) },
            None => command,
        };

        // If this is the last command, get the output
        // Otherwise, create a pipe for the stdout and spawn the command
        // TODO: maybe in future always use pipes and return pipes for the user to deal with
        if last {
            quote!{ let #ident = #command.output()? }
        } else {
            quote!{ let #ident = #command.stdout(std::process::Stdio::piped()).spawn()? }
        }
    }
}