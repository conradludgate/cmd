use std::env::VarError;
use thiserror::Error;

#[derive(Debug, Default, PartialEq)]
pub struct Arg {
    sections: Vec<(String, Var)>,
    buffer: String,
}

impl Arg {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn expand(self) -> Result<String, VarError> {
        let mut builder = Vec::with_capacity(self.sections.len() * 2 + 1);

        for (text, var) in self.sections {
            builder.push(text);
            builder.push(var.expand()?);
        }

        builder.push(self.buffer);

        Ok(builder.join(""))
    }

    pub fn push(&mut self, section: &str) {
        self.buffer.push_str(section);
    }

    pub fn push_char(&mut self, c: char) {
        self.buffer.push(c);
    }

    pub fn push_var(&mut self, var: Var) {
        self.sections.push((self.buffer.clone(), var));
        self.buffer.clear();
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Var {
    name: String,
    option: VarOption,
}

// Variable expansion options, a subset of what's listed here:
// https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
#[derive(Debug, Clone, PartialEq)]
pub enum VarOption {
    None, // $VAR or ${VAR}
    Default(String), // ${VAR-default}
    DefaultIfEmpty(String), // ${VAR:-default}
    IfNotEmpty(String), // ${VAR:+value}
    IfSet(String), // ${VAR+value}
    Offset(isize), // ${VAR:o}
    OffsetLength(isize, isize), // ${VAR:o:l}
    LengthOf, // ${#VAR}
}

impl Default for VarOption {
    fn default() -> Self {
        VarOption::None
    }
}

impl Var {
    pub fn expand(self) -> Result<String, VarError> {
        match (std::env::var(self.name), self.option) {
            (Err(VarError::NotPresent), VarOption::Default(d)) => Ok(d),
            (Err(VarError::NotPresent), VarOption::DefaultIfEmpty(d)) => Ok(d),
            (Ok(v), VarOption::DefaultIfEmpty(d)) => if v.is_empty() { Ok(d) } else { Ok(v) },
            (Ok(v), VarOption::IfNotEmpty(u)) => if v.is_empty() { Ok("".to_string()) } else { Ok(u) },
            (Ok(_), VarOption::IfSet(v)) => Ok(v),
            (Ok(v), VarOption::Offset(offset)) => {
                let l = v.len() as isize;
                let offset = if offset < 0 { l + offset } else { offset };
                let offset = if offset < 0 { 0 } else { offset };
                if offset > l {
                    Ok("".to_string())
                } else  {
                    Ok(v[offset as usize..].to_string())
                }
            },
            (Ok(v), VarOption::OffsetLength(offset, length)) => {
                let l = v.len() as isize;
                let offset = if offset < 0 { l + offset } else { offset };
                let offset = if offset < 0 { 0 } else { offset };
                let end = if length < 0 { l + length } else { offset + length };
                let end = if end > l { l } else { end };
                if offset > l || end > l || end < offset {
                    Ok("".to_string())
                } else  {
                    Ok(v[offset as usize..end as usize].to_string())
                }
            },
            (Ok(v), VarOption::LengthOf) => Ok(format!("{}", v.len())),
            (Err(VarError::NotPresent), VarOption::LengthOf) => Ok("0".to_string()),
            (Ok(v), _) => Ok(v),
            (Err(VarError::NotPresent), _) => Ok("".to_string()),
            (Err(VarError::NotUnicode(os)), _) => Err(VarError::NotUnicode(os)),
        }
    }
}

#[derive(Error, Debug, PartialEq)]
pub enum ParseError {
    #[error("unexpected end of file")]
    EOF,
    #[error("invalid variable name")]
    InvalidVar,
}

use std::iter::Peekable;

pub fn into_arg(s: String) -> Result<Arg, ParseError> {
    let mut chars = s.chars().peekable();

    let mut arg = Arg::new();

    loop {
        match chars.next() {
            Some('$') => {
                arg.push_var(parse_var(&mut chars)?);
            },
            Some('"') => {
                parse_string(&mut chars, &mut arg)?;
            },
            Some('\'') => {
                parse_single(&mut chars, &mut arg)?;
            },
            Some('\\') => {
                match chars.next() {
                    Some('n') => arg.push_char('\n'),
                    Some('$') => arg.push_char('$'),
                    Some('"') => arg.push_char('"'),
                    Some('\'') => arg.push_char('\''),
                    Some('\\') => arg.push_char('\\'),
                    Some(c) => arg.push_char(c),
                    None => return Err(ParseError::EOF),
                }
            }
            Some(c) => arg.push_char(c),
            None => { break; }
        }
    }

    Ok(arg)
}

fn parse_string<I>(s: &mut Peekable<I>, arg: &mut Arg) -> Result<(), ParseError>
where I: Iterator<Item=char> {
    loop {
        match s.next() {
            Some('$') => arg.push_var(parse_var(s)?),
            Some('"') => break,
            Some('\\') => {
                match s.next() {
                    Some('n') => arg.push_char('\n'),
                    Some('$') => arg.push_char('$'),
                    Some('"') => arg.push_char('"'),
                    Some('\\') => arg.push_char('\\'),
                    Some(c) => arg.push_char(c),
                    None => return Err(ParseError::EOF),
                }
            }
            Some(c) => arg.push_char(c),
            _ => return Err(ParseError::EOF),
        }
    }
    Ok(())
}

fn parse_single<I>(s: &mut Peekable<I>, arg: &mut Arg) -> Result<(), ParseError>
where I: Iterator<Item=char> {
    loop {
        match s.next() {
            Some('\'') => break,
            Some('\\') => {
                match s.next() {
                    Some('n') => arg.push_char('\n'),
                    Some('$') => arg.push_char('$'),
                    Some('\'') => arg.push_char('\''),
                    Some('\\') => arg.push_char('\\'),
                    Some(c) => arg.push_char(c),
                    None => return Err(ParseError::EOF),
                }
            }
            Some(c) => arg.push_char(c),
            _ => return Err(ParseError::EOF),
        }
    }
    Ok(())
}

const VALID_VAR_CHARS: &str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_0123456789";

fn parse_var<I>(s: &mut Peekable<I>) -> Result<Var, ParseError>
where I: Iterator<Item=char> {
    if let Some(c) = s.peek() { 
        if *c == '{' {
            s.next();

            let mut buffer = vec![];

            loop {
                match s.next() {
                    Some(c) => if c == '}' { break } else { buffer.push(c) },
                    None => return Err(ParseError::EOF),
                }
            }

            return Ok(parse_var_options(&mut buffer.into_iter().peekable())?);
        }
    }

    if s.peek().is_none() {
        return Err(ParseError::EOF);
    }

    let first = s.next().unwrap();
    if !VALID_VAR_CHARS.contains(first) {
        return Err(ParseError::InvalidVar);
    }

    let mut name = String::new();
    name.push(first);

    while let Some(c) = s.peek() {
        if !VALID_VAR_CHARS.contains(*c) {
            break;
        }
        name.push(s.next().unwrap());
    }

    Ok(Var{
        name,
        option: VarOption::None,
    })
}

fn parse_var_options<I>(s: &mut Peekable<I>) -> Result<Var, ParseError>
where I: Iterator<Item=char> {
    if s.peek().is_none() {
        return Err(ParseError::EOF);
    }

    let mut name = String::new();
    let mut length = false;

    let first = s.next().unwrap();
    if first == '#' {
        length = true;
    } else if !VALID_VAR_CHARS.contains(first) {
        return Err(ParseError::InvalidVar);
    } else {
        name.push(first);
    }

    while let Some(c) = s.peek() {
        if !VALID_VAR_CHARS.contains(*c) {
            break;
        }
        name.push(s.next().unwrap());
    }

    if length && !s.peek().is_none() {
        return Err(ParseError::InvalidVar);
    } else if length && name.len() == 0 {
        return Err(ParseError::EOF);
    } else if length {
        return Ok(Var{
            name,
            option: VarOption::LengthOf,
        });
    }

    // None, // $VAR or ${VAR}
    // Default(String), // ${VAR-default}
    // DefaultIfEmpty(String), // ${VAR:-default}
    // IfNotEmpty(String), // ${VAR:+value}
    // IfSet(String), // ${VAR+value}
    // Offset(isize), // ${VAR:o}
    // OffsetLength(isize, isize), // ${VAR:o:l}
    // LengthOf, // ${#VAR}

    let c = match s.next() {
        None => return Ok(Var{
            name,
            option: VarOption::None,
        }), // ${VAR}
        Some(c) => c,
    };

    match c {
        '-' => {
            Ok(Var{
                name,
                option: VarOption::Default(s.collect()),
            })
        }, // "${VAR-default}"
        '+' => {
            Ok(Var{
                name,
                option: VarOption::IfSet(s.collect()),
            })
        }, // "${VAR+value}"
        ':' => {
            match s.peek().map(|&c| c) {
                Some('-') => {
                    s.next();
                    Ok(Var{
                        name,
                        option: VarOption::DefaultIfEmpty(s.collect()),
                    })
                }, // "${VAR:-default}"
                Some('+') => {
                    s.next();
                    Ok(Var{
                        name,
                        option: VarOption::IfNotEmpty(s.collect()),
                    })
                } // "${VAR:+value}"
                Some(_) => {
                    let s: String = s.collect();
                    let sections: Vec<isize> = s.split(':')
                        .flat_map(|n| n
                            .trim()
                            .to_string()
                            .parse::<isize>()
                        ).collect();
                    match sections[..] {
                        [offset] => Ok(Var{
                            name,
                            option: VarOption::Offset(offset),
                        }),
                        [offset, length] => Ok(Var{
                            name,
                            option: VarOption::OffsetLength(offset, length),
                        }),
                        _ => Err(ParseError::InvalidVar),
                    }
                } // "${VAR:o}" or "${VAR:o:l}" depending on count
                _ => {
                    Err(ParseError::InvalidVar)
                },
            }
        }
        _ => {
            Err(ParseError::InvalidVar)
        },
    }
}
#[derive(Error, Debug)]
pub enum ParseExpandError {
    #[error("expanding args")]
    ExpandError(#[from] VarError),
    #[error("parsing args")]
    ParseError(#[from] ParseError),
}

pub fn parse_expand(s: String) -> Result<String, ParseExpandError> {
    Ok(into_arg(s)?.expand()?)
}

#[cfg(test)]
mod tests {
    use std::env::{set_var, remove_var, var, VarError};
    use crate::env::*;

    const VAR_NAME: &str = "TEST_FOO_BAR";
    const VAR_NAME_EMPTY: &str = "TEST_FOO_BAR_EMPTY";
    const VAR_NAME_NULL: &str = "TEST_FOO_BAR_NULL";
    const VAR_VAL: &str = "test_foo_bar";
    const VAR_DEFAULT: &str = "default_value";

    fn setup() {
        set_var(VAR_NAME, VAR_VAL);
        assert_eq!(var(VAR_NAME).expect("could not get var"), VAR_VAL);

        set_var(VAR_NAME_EMPTY, "");
        assert_eq!(var(VAR_NAME_EMPTY).expect("could not get var"), "");

        remove_var(VAR_NAME_NULL);
        assert_eq!(var(VAR_NAME_NULL), Err(VarError::NotPresent));
    }

    use VarOption::*;

    #[test]
    fn expand_arg() {

        let a = 'a' as u8;
        let sections: Vec<(String, Var)> = (0u8..4).map(|i| {
            let name = format!("{}{}", VAR_NAME, i);
            let val = format!("{}{}", VAR_VAL, i);
            set_var(&name, &val);
            assert_eq!(var(&name).expect("could not get var"), val);

            (
                format!("{}", (a + i) as char),
                Var {
                    name: name,
                    option: None,
                },
            )
        }).collect();

        let arg = Arg{
            sections: sections.clone(),
            buffer: "".to_string(),
        };
        assert_eq!(arg.expand(), Ok(format!("a{v}0b{v}1c{v}2d{v}3", v=VAR_VAL)));

        let arg = Arg{
            sections: sections.clone(),
            buffer: "e".to_string(),
        };
        assert_eq!(arg.expand(), Ok(format!("a{v}0b{v}1c{v}2d{v}3e", v=VAR_VAL)));
    }

    macro_rules! test_expand {
        ($context:expr, $option:expr, $val:expr) => {
            test_expand!(VAR_NAME, $context, $option, $val);
        };

        (empty, $context:expr, $option:expr, $val:expr) => {
            test_expand!(VAR_NAME_EMPTY, $context, $option, $val);
        };

        (null, $context:expr, $option:expr, $val:expr) => {
            test_expand!(VAR_NAME_NULL, $context, $option, $val);
        };

        ($var:expr, $context:expr, $option:expr, $val:expr) => {
            let v = Var {
                name: $var.to_string(),
                option: $option,
            };
            let expanded = v.expand();
            let expected = Ok($val.to_string());
            assert_eq!(expanded, expected, "\nfor test: {}", $context);
        };

        ($context:expr, $option:expr, err $err:expr) => {
            let v = Var {
                name: VAR_NAME.to_string(),
                option: $option,
            };
            let expanded = v.expand();
            let expected = Err($err);
            assert_eq!(expanded, expected, "\nfor test: {}", $context);
        };
    }

    #[test]
    #[should_panic]
    fn test_expand_ok() {
        setup();
        test_expand!("panic", None, "");
    }

    #[test]
    #[should_panic]
    fn test_expand_err() {
        setup();
        test_expand!("panic", None, err VarError::NotPresent);
    }

    #[test]
    fn expand_var_not_empty() {
        setup();

        let len = VAR_VAL.len();
        let l = len as isize;

        test_expand!("none", None, VAR_VAL);
        test_expand!("default", Default(VAR_DEFAULT.to_string()), VAR_VAL);
        test_expand!("default_if_empty", DefaultIfEmpty(VAR_DEFAULT.to_string()), VAR_VAL);
        test_expand!("if_not_empty", IfNotEmpty(VAR_DEFAULT.to_string()), VAR_DEFAULT);
        test_expand!("if_set", IfSet(VAR_DEFAULT.to_string()), VAR_DEFAULT);

        test_expand!("offset", Offset(5), &VAR_VAL[5..]);
        test_expand!("offset_outofrange", Offset(l), "");
        test_expand!("offset_negative", Offset(-3), &VAR_VAL[len-3..]);
        test_expand!("offset_negative_outofrange", Offset(-l-1), VAR_VAL);

        test_expand!("offset_length", OffsetLength(5, 3), &VAR_VAL[5..8]);
        test_expand!("offset_negative_length", OffsetLength(-7, 3), &VAR_VAL[len-7..len-4]);
        test_expand!("offset_length_negative", OffsetLength(5, -4), &VAR_VAL[5..len-4]);
        test_expand!("offset_negative_length_negative", OffsetLength(-7, -4), &VAR_VAL[len-7..len-4]);
        test_expand!("offset_outofrange_length", OffsetLength(l, 3), "");
        test_expand!("offset_length_extra", OffsetLength(5, 8), &VAR_VAL[5..]);
        test_expand!("offset_negative_outofrange_length", OffsetLength(-l-1, 3), &VAR_VAL[..3]);
        test_expand!("offset_length_zero", OffsetLength(5, 0), "");
        test_expand!("offset_negative_length_zero", OffsetLength(-7, 0), "");
        test_expand!("offset_outofrange_length_zero", OffsetLength(l, 0), "");
        test_expand!("offset_negative_outofrange_length_zero", OffsetLength(-l-1, 0), "");

        test_expand!("length_of", LengthOf, format!("{}", l));
        
        // empty
        test_expand!(empty, "none", None, "");
        test_expand!(empty, "default", Default(VAR_DEFAULT.to_string()), "");
        test_expand!(empty, "default_if_empty", DefaultIfEmpty(VAR_DEFAULT.to_string()), VAR_DEFAULT);
        test_expand!(empty, "if_not_empty", IfNotEmpty(VAR_DEFAULT.to_string()), "");
        test_expand!(empty, "if_set", IfSet(VAR_DEFAULT.to_string()), VAR_DEFAULT);

        test_expand!(empty, "offset", Offset(5), "");
        test_expand!(empty, "offset_negative", Offset(-5), "");

        test_expand!(empty, "offset_length", OffsetLength(5, 3), "");
        test_expand!(empty, "offset_negative_length", OffsetLength(-7, 3), "");
        test_expand!(empty, "offset_length_negative", OffsetLength(5, -4), "");
        test_expand!(empty, "offset_negative_length_negative", OffsetLength(-7, -4), "");

        test_expand!(empty, "length_of", LengthOf, "0");
   
        // null
        test_expand!(null, "none", None, "");
        test_expand!(null, "default", Default(VAR_DEFAULT.to_string()), VAR_DEFAULT);
        test_expand!(null, "default_if_empty", DefaultIfEmpty(VAR_DEFAULT.to_string()), VAR_DEFAULT);
        test_expand!(null, "if_not_empty", IfNotEmpty(VAR_DEFAULT.to_string()), "");
        test_expand!(null, "if_set", IfSet(VAR_DEFAULT.to_string()), "");

        test_expand!(null, "offset", Offset(5), "");
        test_expand!(null, "offset_negative", Offset(-5), "");

        test_expand!(null, "offset_length", OffsetLength(5, 3), "");
        test_expand!(null, "offset_negative_length", OffsetLength(-7, 3), "");
        test_expand!(null, "offset_length_negative", OffsetLength(5, -4), "");
        test_expand!(null, "offset_negative_length_negative", OffsetLength(-7, -4), "");

        test_expand!(null, "length_of", LengthOf, "0");
    }

    macro_rules! test_parse_var_options{
        ($context:expr, $case:expr, $name:expr, $option:expr) => {
            let got = parse_var_options(&mut $case.to_string().chars().peekable());
            let expected = Ok(Var{
                name: $name.to_string(),
                option: $option,
            });
            assert_eq!(got, expected, "\nfor test: {}", $context);
        };

        ($context:expr, $case:expr, err $err:expr) => {
            let got = parse_var_options(&mut $case.to_string().chars().peekable());
            let expected = Err($err);
            assert_eq!(got, expected, "\nfor test: {}", $context);
        };
    }

    #[test]
    #[should_panic]
    fn test_parse_var_options_ok_panic() {
        test_parse_var_options!("panic", "$", "$", None);
    }

    #[test]
    #[should_panic]
    fn test_parse_var_options_err_panic() {
        test_parse_var_options!("panic", "FOO", err ParseError::InvalidVar);
    }

    #[test]
    fn parse_var_options_ok() {
        test_parse_var_options!("basic", "FOO", "FOO", None);
        test_parse_var_options!("default", "FOO-default", "FOO", Default("default".to_string()));
        test_parse_var_options!("default if empty", "FOO:-default", "FOO", DefaultIfEmpty("default".to_string()));
        test_parse_var_options!("if set", "FOO+value", "FOO", IfSet("value".to_string()));
        test_parse_var_options!("if not empty", "FOO:+value", "FOO", IfNotEmpty("value".to_string()));
        test_parse_var_options!("if not empty", "FOO:+value", "FOO", IfNotEmpty("value".to_string()));

        test_parse_var_options!("offset", "FOO:5", "FOO", Offset(5));
        test_parse_var_options!("offset negative", "FOO: -5", "FOO", Offset(-5));

        test_parse_var_options!("offset length", "FOO:5:3", "FOO", OffsetLength(5, 3));
        test_parse_var_options!("offset negative length", "FOO: -5:3", "FOO", OffsetLength(-5, 3));
        test_parse_var_options!("offset length negative", "FOO:5:-3", "FOO", OffsetLength(5, -3));
        test_parse_var_options!("offset negative length negative", "FOO: -5:-3", "FOO", OffsetLength(-5, -3));

        test_parse_var_options!("length of", "#FOO", "FOO", LengthOf);
    }

    #[test]
    fn parse_var_options_err() {
        test_parse_var_options!("empty", "", err ParseError::EOF);
        test_parse_var_options!("bad_first", "$", err ParseError::InvalidVar);
        test_parse_var_options!("empty length of", "#", err ParseError::EOF);
        test_parse_var_options!("bad length of", "#$", err ParseError::InvalidVar);
        test_parse_var_options!("empty after :", "FOO:", err ParseError::InvalidVar);
        test_parse_var_options!("invalid after :", "FOO:_", err ParseError::InvalidVar);
        test_parse_var_options!("too many numbers after :", "FOO:4:4:4", err ParseError::InvalidVar);
        test_parse_var_options!("unknown after var", "FOO$", err ParseError::InvalidVar);
    }

    macro_rules! test_parse_var{
        ($context:expr, $case:expr, $name:expr) => {
            let case = $case.to_string();
            let mut chars = case.chars().peekable();
            let got = parse_var(&mut chars);
            let expected = Ok(Var{
                name: $name.to_string(),
                option: None,
            });
            assert_eq!(got, expected, "\nfor test: {}", $context);
            let excess: String = chars.collect();
            assert_eq!(excess, "", "\nfor excess test: {}", $context);
        };

        ($context:expr, $case:expr, $name:expr, excess $excess:expr) => {
            let case = $case.to_string();
            let mut chars = case.chars().peekable();
            let got = parse_var(&mut chars);
            let expected = Ok(Var{
                name: $name.to_string(),
                option: None,
            });
            assert_eq!(got, expected, "\nfor test: {}", $context);
            let excess: String = chars.collect();
            assert_eq!(excess, $excess, "\nfor excess test: {}", $context);
        };

        ($context:expr, $case:expr, $name:expr, $option:expr) => {
            let got = parse_var(&mut $case.to_string().chars().peekable());
            let expected = Ok(Var{
                name: $name.to_string(),
                option: $option,
            });
            assert_eq!(got, expected, "\nfor test: {}", $context);
        };

        ($context:expr, $case:expr, err $err:expr) => {
            let got = parse_var(&mut $case.to_string().chars().peekable());
            let expected = Err($err);
            assert_eq!(got, expected, "\nfor test: {}", $context);
        };
    }

    #[test]
    #[should_panic]
    fn test_parse_var_with_option_ok_panic() {
        test_parse_var!("panic", "{$}", "$", None);
    }

    #[test]
    #[should_panic]
    fn test_parse_var_ok_panic() {
        test_parse_var!("panic", "{$}", "$");
    }

    #[test]
    #[should_panic]
    fn test_parse_var_err_panic() {
        test_parse_var!("panic", "{FOO}-", err ParseError::InvalidVar);
    }

    #[test]
    fn parse_var_with_option_ok() {
        test_parse_var!("basic", "{FOO}", "FOO", None);
        test_parse_var!("default", "{FOO-default}", "FOO", Default("default".to_string()));
        test_parse_var!("default if empty", "{FOO:-default}", "FOO", DefaultIfEmpty("default".to_string()));
        test_parse_var!("if set", "{FOO+value}", "FOO", IfSet("value".to_string()));
        test_parse_var!("if not empty", "{FOO:+value}", "FOO", IfNotEmpty("value".to_string()));
        test_parse_var!("if not empty", "{FOO:+value}", "FOO", IfNotEmpty("value".to_string()));

        test_parse_var!("offset", "{FOO:5}", "FOO", Offset(5));
        test_parse_var!("offset negative", "{FOO: -5}", "FOO", Offset(-5));

        test_parse_var!("offset length", "{FOO:5:3}", "FOO", OffsetLength(5, 3));
        test_parse_var!("offset negative length", "{FOO: -5:3}", "FOO", OffsetLength(-5, 3));
        test_parse_var!("offset length negative", "{FOO:5:-3}", "FOO", OffsetLength(5, -3));
        test_parse_var!("offset negative length negative", "{FOO: -5:-3}", "FOO", OffsetLength(-5, -3));

        test_parse_var!("length of", "{#FOO}", "FOO", LengthOf);
    }

    #[test]
    fn parse_var_with_option_err() {
        test_parse_var!("empty", "{}", err ParseError::EOF);
        test_parse_var!("bad_first", "{$}", err ParseError::InvalidVar);
        test_parse_var!("empty length of", "{#}", err ParseError::EOF);
        test_parse_var!("bad length of", "{#$}", err ParseError::InvalidVar);
        test_parse_var!("empty after :", "{FOO:}", err ParseError::InvalidVar);
        test_parse_var!("invalid after :", "{FOO:_}", err ParseError::InvalidVar);
        test_parse_var!("too many numbers after :", "{FOO:4:4:4}", err ParseError::InvalidVar);
        test_parse_var!("unknown after var", "{FOO$}", err ParseError::InvalidVar);
    }

    #[test]
    fn parse_var_ok() {
        test_parse_var!("simple", "FOO", "FOO");
        test_parse_var!("extra", "FOO-", "FOO", excess "-");
    }

    #[test]
    fn into_arg_ok() {
        let arg = into_arg("foo bar \"baz $var\"\\nfoo bar 'baz $var'\\n".to_string());
        let mut expected = Arg::new();
        expected.push("foo bar baz ");
        expected.push_var(Var{name: "var".to_string(), option: None});
        expected.push("\nfoo bar baz $var\n");
        assert_eq!(arg, Ok(expected));
    }
}