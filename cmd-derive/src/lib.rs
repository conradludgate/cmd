extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as Stream, Span};
use proc_macro_hack::proc_macro_hack;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Expr, Ident};
use syn::parse::{Parse, ParseStream, Result};
use syn::punctuated::Punctuated;
use syn::Token;

// fn str_expr(s: &str) -> Expr {
//     Expr::Lit(
//         syn::ExprLit {
//             attrs: vec![],
//             lit: syn::Lit::Str(
//                 syn::LitStr::new(s, Span::call_site())
//             ),
//         }
//     )
// }

fn str_expr(s: &str, span: Span) -> Expr {
    Expr::Lit(
        syn::ExprLit {
            attrs: vec![],
            lit: syn::Lit::Str(
                syn::LitStr::new(s, span)
            ),
        }
    )
}

#[proc_macro_hack]
pub fn cmd(input: TokenStream) -> TokenStream {
    let root = parse_macro_input!(input as Cmd);
    
    TokenStream::from(root.into_token_stream())
}

struct Cmd {
    envs: Option<Expr>,
    commands: Vec<Command>,
    stdin: Option<Expr>,
}

struct Command {
    span: Span,
    name: Expr,
    args: Vec<Expr>
}

impl Parse for Cmd {
    fn parse(input: ParseStream) -> Result<Self> {

        let commands = Punctuated::<Command, Token![|]>::parse_terminated(input)?;
        
        Ok(Cmd{
            envs: None,
            commands: commands.into_iter().collect(),
            stdin: None
        })
    }
}

fn convert_expr(expr: Expr) -> Expr {
    match expr {
        Expr::Path(path) => {
            match path.path.get_ident() {
                Some(ident) => str_expr(&format!("{}", ident), ident.span()),
                None => Expr::Path(path),
            }
        },
        _ => expr,
    }
}

impl Parse for Command {
    fn parse(input: ParseStream) -> Result<Self> {
        // ls -la => name: "ls", args: vec!["-la"]
        // grep "Cargo" => name: "grep", args: vec!["Cargo"]
        // grep #cargo => name: "grep", args: vec![cargo]

        // TODO: Will require lots of span checking
        // "ls -la" will read in the stream as "ls" "-" "la",
        // But the span locations will show that "-" is next to "la"

        let name_ident = input.parse::<Ident>()?;
        let name = str_expr(&format!("{}", name_ident), name_ident.span());

        let mut args = Vec::<Expr>::new();
        loop {
            if input.is_empty() || input.peek(Token![|]) {
                break;
            }

            let expr = convert_expr(input.parse::<Expr>()?);

            args.push(expr);
        }

        Ok(Command{
            span: name_ident.span(),
            name: name,
            args: args,
        })

        // Err(syn::parse::Error::new(Span::call_site(), "not_implemented"))
    }
}

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