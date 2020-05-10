#![feature(proc_macro_span)]
#![feature(proc_macro_diagnostic)]

use proc_macro2::{TokenStream, TokenTree, Punct, Group, Ident, Span};
use proc_macro::{Diagnostic, TokenStream as TokenStream1};
use boolinator::Boolinator;
use std::iter::Peekable;
use quote::{quote, quote_spanned, ToTokens};

// #[proc_macro_hack]
#[proc_macro]
pub fn cmd(input: TokenStream1) -> TokenStream1 {
    let c = parse_cmd(TokenStream::from(input))
        .map_err(|e| e.emit())
        .expect("Error parsing commands");
    c.into_token_stream().into()
}

pub(crate) struct Cmd {
    commands: Vec<Command>,
}

struct Command {
    span: Span,
    terms: Vec<Expr>
}

pub(crate) fn parse_cmd(input: TokenStream) -> Result<Cmd, Diagnostic> {
    let mut tokens = input.into_iter().peekable();

    let mut commands: Vec<Command> = vec![];
    loop {
        let command = parse_command(&mut tokens)?;
        commands.push(command);

        match next_punct_is(&mut tokens, '|') {
            Ok(_) => { tokens.next(); },
            Err(NextError::EOF) => break,
            _ => return Err(
                span_remaining(&mut tokens)
                    .unwrap()
                    .error("expected EOF or | after end of command")
            ),
        }
    }

    Ok(Cmd{
        commands,
    })
}

fn parse_command<I>(input: &mut Peekable<I>) -> Result<Command, Diagnostic>
where I: Iterator<Item=TokenTree>
{
    let first_term = parse_term(input)?;
    let mut span = first_term.span();
    let mut terms = vec![first_term];

    loop {
        if next_punct_is(input, '|').is_ok() || is_eof(input)  {
            break;
        }

        let term = parse_term(input)?;
        let term_span = term.span();
        terms.push(term);

        span = span.join(term_span).ok_or(
            span.unwrap().error("internal error: could not join spans")
        )?;
    }

    Ok(Command{span, terms})
}

fn is_eof<I>(input: &mut Peekable<I>) -> bool
where I: Iterator<Item=TokenTree>
{
    input.peek().is_none()
}

enum Expr {
    Literal(String, Span),
    Expr(syn::Expr, Span),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Literal(_, span) => *span,
            Expr::Expr(_, span) => *span,
        }
    }
}

impl ToTokens for Expr {
    fn to_tokens(&self, t: &mut TokenStream) {
        t.extend(match self {
            Expr::Literal(str, span) => quote_spanned!{*span=> #str },
            Expr::Expr(e, span) => quote_spanned! {*span=> format!("{}", #e) },
        })
    }
}

const ALONE_HASH_ERROR: &str = "'#' must not be alone. use '##' if you meant to pass a literal '#' symbol.
To pass two or more, only one extra '#' must be supplied
Example: `cmd!(echo ####a###)` is equiavalent to `$ echo ###a###`";

const UNEXPECTED_HASH_ERROR: &str = "'#' should be followed by an ident, an expression wrapped in any or (), {}, [] or another #
Example:
cmd!(echo ##) // echo #

let hello = \"hello world\";
cmd!(echo #hello) // echo \"hello world\"

let hello = \"hello\"
let world = \"world\";
cmd!(echo #{[hello, world].join(\" \")}) // echo \"hello world\"
";

fn parse_term<I>(input: &mut Peekable<I>) -> Result<Expr, Diagnostic>
where I: Iterator<Item=TokenTree>
{
    if next_punct_is(input, '#').is_ok() {
        let p = punct(input)
            .expect("internal error: peeked '#' but could not parse punct. please file a bug report");
        
        match input.peek() {
            None => return Err(p.span().unwrap().error(ALONE_HASH_ERROR)),
            Some(tt) => {
                let span = to_span(tt);

                if !joined(p.span(), span) {
                    return Err(p.span().unwrap().error(ALONE_HASH_ERROR));
                }

                match tt {
                    TokenTree::Punct(p) => 
                        match p.as_char() {
                            '#' => {}, // Do nothing. Valid term
                            _ => return Err(p.span().unwrap().error(UNEXPECTED_HASH_ERROR)),
                        },
                    TokenTree::Ident(_) => {
                        return parse_ident(ident(input)
                            .expect("internal error: peeked ident but could not parse ident. please file a bug report"))
                    }
                    TokenTree::Group(_) => {
                        return parse_expr(group(input)
                            .expect("internal error: peeked group but could not parse group. please file a bug report"))
                    }
                    _ => return Err(p.span().unwrap().error(UNEXPECTED_HASH_ERROR)),
                }
                
            }
        }
    }

    parse_literal(input).map(|(lit, span)| Expr::Literal(lit, span))  
}

fn parse_literal<I>(input: &mut Peekable<I>) -> Result<(String, Span), Diagnostic>
where I: Iterator<Item=TokenTree> {
    // We check for EOF in parse_command so this should not return None
    // Unless we're parsing a Group like `{}`, which will check the literal before
    // joining the span
    let first = match input.next() {
        None => return Ok((String::new(), Span::call_site())),
        Some(term) => term,
    };

    let (mut lit, mut span) = to_string_span(first)?;

    loop {
        let peek = match input.peek() {
            Some(tt) => tt,
            None => break,
        };

        if !joined(span, peek.span()) {
            break;
        }

        let (new_lit, new_span) = to_string_span(input.next()
            .expect("internal error: peeked token but could not parse token. please file a bug report"))?;

        lit = [lit, new_lit].join("");
        span = span.join(new_span)
            .ok_or(span.unwrap().error("internal error: could not join spans"))?;
    }

    Ok((lit, span))
}

fn to_string_span(tt: TokenTree) -> Result<(String, Span), Diagnostic> {
    match tt {
        TokenTree::Literal(l) => Ok((l.to_string(), l.span())),
        TokenTree::Ident(i) => Ok((i.to_string(), i.span())),
        TokenTree::Punct(p) => Ok((p.to_string(), p.span())),
        TokenTree::Group(g) => {
            let mut tokens = g.stream().into_iter().peekable();
            // TODO: refactor. This currently gets confused.
            // cmd!(echo {a | foo }) will run `echo {a}`
            let (lit, _) = parse_literal(&mut tokens)?;
            let lit = match g.delimiter() {
                proc_macro2::Delimiter::None => lit,
                proc_macro2::Delimiter::Parenthesis => format!("({})", lit),
                proc_macro2::Delimiter::Brace => format!("{{{}}}", lit),
                proc_macro2::Delimiter::Bracket => format!("<{}>", lit),
            };
            Ok((lit, g.span()))
        }
    }
}

fn parse_ident(i: Ident) -> Result<Expr, Diagnostic> {
    use syn::{PathSegment, PathArguments, ExprPath, Path, token::Colon2, punctuated::Punctuated};
    let mut p: Punctuated::<PathSegment, Colon2> = Punctuated::new();

    let span = i.span();

    p.push_value(PathSegment{
        ident: i,
        arguments: PathArguments::None,
    });

    Ok(Expr::Expr(
        syn::Expr::Path(
            ExprPath {
                attrs: vec![],
                qself: None,
                path: Path {
                    leading_colon: None,
                    segments: p,
                }
            }
        ),
        span,
    ))
}

fn parse_expr(g: Group) -> Result<Expr, Diagnostic> {
    let expr = syn::parse2::<syn::Expr>(g.stream())
        .map_err(|err| err.span().unwrap().error(err.to_string()))?;
    Ok(Expr::Expr(expr, g.span()))
}

#[derive(Debug)]
enum NextError {
    EOF,
    NotFound,
}

fn to_span(tt: &TokenTree) -> Span {
    match tt {
        TokenTree::Literal(l) => l.span(),
        TokenTree::Group(g) => g.span(),
        TokenTree::Punct(p) => p.span(),
        TokenTree::Ident(i) => i.span(),
    }
}

fn punct<I>(tokens: &mut I) -> Result<Punct, NextError>
where I: Iterator<Item=TokenTree>
{
    match tokens.next().ok_or(NextError::EOF)? {
        TokenTree::Punct(p) => Ok(p),
        _ => Err(NextError::NotFound)
    }
}

fn next_punct_is<I>(tokens: &mut Peekable<I>, is: char) -> Result<Span, NextError>
where I: Iterator<Item=TokenTree>
{
    match tokens.peek().ok_or(NextError::EOF)? {
        TokenTree::Punct(p) => (p.as_char() == is).as_result(p.span(), NextError::NotFound),
        _ => Err(NextError::NotFound),
    }
}

fn ident<I>(tokens: &mut I) -> Result<Ident, NextError>
where I: Iterator<Item=TokenTree>
{
    match tokens.next().ok_or(NextError::EOF)? {
        TokenTree::Ident(i) => Ok(i),
        _ => Err(NextError::NotFound),
    }
}

fn group<I>(tokens: &mut I) -> Result<Group, NextError>
where I: Iterator<Item=TokenTree>
{
    match tokens.next().ok_or(NextError::EOF)? {
        TokenTree::Group(g) => Ok(g),
        _ => Err(NextError::NotFound),
    }
}

fn span_remaining<I>(tokens: &mut I) -> Span 
where I: Iterator<Item=TokenTree>
{
    let mut spans = tokens
        .map(|x| x.span());

	let mut result = match spans.next() {
        Some(span) => span,
        None => Span::call_site(),
    };
    
	for span in spans {
		result = match result.join(span) {
			None => return Span::from(result),
			Some(span) => span,
		}
    }
    result
}
    
fn joined(lhs: Span, rhs: Span) -> bool {
    let a = lhs.end();
    let b = rhs.start();
    a == b
}

impl ToTokens for Cmd {
    fn to_tokens(&self, tokens: &mut TokenStream) {
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
        let mut stdin = None;
        let cmds: Vec<TokenStream> = self.commands.iter().zip(0..).map(|(cmd, i)| {
            
            let ident = Ident::new(&format!("x{}", i), cmd.span);

            let command = cmd.to_tokens(ident.clone(), stdin.clone(), i == self.commands.len() - 1);

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
    fn to_tokens(&self, ident: Ident, stdin: Option<TokenStream>, last: bool) -> TokenStream {
        let mut terms = (*self.terms).into_iter();
        let first = match terms.next() {
            None => return quote!{},
            Some(term) => term,
        };

        let command = terms.fold(
            quote!{ std::process::Command::new(#first) }, // Create the command
            |command, arg| quote!{ #command.arg(#arg) }, // and add all the args
        );

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
