use crate::error::{EBNFError, EBNFResult};
use pest::iterators::{Pair, Pairs};
#[allow(unused_imports)]
use pest::Parser;
use std::{convert::TryFrom, hash::Hash};

trait FromRule: Sized {
    fn try_from(pair: Pair<Rule>) -> EBNFResult<Self>;
}

#[derive(Parser)]
#[grammar = "./ebnf.pest"]
pub struct InnerParser;

pub type Rrule = Rule;

impl InnerParser {
    pub fn new(raw: &str) -> EBNFResult<Syntax> {
        let pair = InnerParser::parse(Rule::syntax, raw)?.next().unwrap();
        let syntax = Syntax::try_from(pair)?;
        Ok(syntax)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Syntax {
    rules: Vec<SyntaxRule>,
}

macro_rules! impl_form_str {
    ($ty:ty, $val:expr) => {
        impl std::str::FromStr for $ty {
            type Err = EBNFError;

            fn from_str(raw: &str) -> Result<Self, Self::Err> {
                if let Some(pair) = InnerParser::parse(Rule::syntax, raw)?.next() {
                    let syntax = Self::try_from(pair)?;
                    Ok(syntax)
                } else {
                    Err(EBNFError::NoTokens)
                }
            }
        }
    };
}

impl_form_str!(Syntax, Rule::syntax);

impl<'r> TryFrom<Pair<'r, Rule>> for Syntax {
    type Error = EBNFError;
    fn try_from(pair: Pair<'r, Rule>) -> EBNFResult<Syntax> {
        let inner = pair.into_inner();
        let result = Syntax {
            rules: inner
                // TODO: We should really check that EOI only exists in the beginning.
                .filter(|rule| rule.as_rule() != Rule::EOI)
                .map(|rule| SyntaxRule::try_from(rule))
                .collect::<EBNFResult<Vec<SyntaxRule>>>()?,
        };
        Ok(result)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct MetaIdentifier(String);

impl_form_str!(MetaIdentifier, Rule::meta_identifier);

impl<'r> TryFrom<Pair<'r, Rule>> for MetaIdentifier {
    type Error = EBNFError;
    fn try_from(pair: Pair<Rule>) -> EBNFResult<MetaIdentifier> {
        match pair.as_rule() {
            Rule::meta_identifier => Ok(MetaIdentifier(String::from(pair.as_str()))),
            o => Err(EBNFError::UnexpectedRules(vec![(
                o,
                pair.as_str().to_string(),
            )])),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntaxRule {
    identifier: MetaIdentifier,
    definitions: DefinitionList,
}

impl_form_str!(SyntaxRule, Rule::syntax_rule);

impl<'r> TryFrom<Pair<'r, Rule>> for SyntaxRule {
    type Error = EBNFError;
    fn try_from(pair: Pair<Rule>) -> EBNFResult<SyntaxRule> {
        let mut pair = pair.into_inner();
        match (pair.next(), pair.next(), pair.next(), pair.next()) {
            (
                Some(meta_identifier),
                Some(defining_symbol),
                Some(definition_list),
                Some(terminator_symbol),
            ) => match (
                meta_identifier.as_rule(),
                defining_symbol.as_rule(),
                definition_list.as_rule(),
                terminator_symbol.as_rule(),
            ) {
                (
                    Rule::meta_identifier,
                    Rule::defining_symbol,
                    Rule::definition_list,
                    Rule::terminator_symbol,
                ) => Ok(SyntaxRule {
                    identifier: MetaIdentifier::try_from(meta_identifier)?,
                    definitions: DefinitionList::try_from(definition_list)?,
                }),
                (a, b, c, d) => Err(EBNFError::UnexpectedRules(vec![
                    (a, meta_identifier.as_str().to_string()),
                    (b, defining_symbol.as_str().to_string()),
                    (c, definition_list.as_str().to_string()),
                    (d, terminator_symbol.as_str().to_string()),
                ])),
            },
            (a, b, c, d) => Err(EBNFError::InsufficientTokens(vec![
                a.map(|p| p.as_rule()),
                b.map(|p| p.as_rule()),
                c.map(|p| p.as_rule()),
                d.map(|p| p.as_rule()),
            ])),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct DefinitionList(Vec<SingleDefinition>);

impl_form_str!(DefinitionList, Rule::definition_list);

impl<'r> TryFrom<Pair<'r, Rule>> for DefinitionList {
    type Error = EBNFError;
    fn try_from(pair: Pair<Rule>) -> EBNFResult<DefinitionList> {
        let pair = pair.into_inner();
        Ok(DefinitionList(
            pair.step_by(2)
                .map(|definition_list| SingleDefinition::try_from(definition_list))
                .collect::<EBNFResult<Vec<SingleDefinition>>>()?,
        ))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SingleDefinition(Vec<SyntacticTerm>);

impl_form_str!(SingleDefinition, Rule::single_definition);

impl<'r> TryFrom<Pair<'r, Rule>> for SingleDefinition {
    type Error = EBNFError;
    fn try_from(pair: Pair<Rule>) -> EBNFResult<SingleDefinition> {
        let pair = pair.into_inner();
        Ok(SingleDefinition(
            pair.step_by(2)
                .map(|syntactic_term| SyntacticTerm::try_from(syntactic_term))
                .collect::<EBNFResult<Vec<SyntacticTerm>>>()?,
        ))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntacticException(String);

impl_form_str!(SyntacticException, Rule::syntactic_expression);

impl<'r> TryFrom<Pair<'r, Rule>> for SyntacticException {
    type Error = EBNFError;
    fn try_from(pair: Pair<Rule>) -> EBNFResult<SyntacticException> {
        let pair = pair.into_inner();
        Ok(SyntacticException(String::from(pair.as_str())))
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntacticTerm {
    factor: SyntacticFactor,
    except: Option<SyntacticException>,
}

impl_form_str!(SyntacticTerm, Rule::syntactic_term);

impl<'r> TryFrom<Pair<'r, Rule>> for SyntacticTerm {
    type Error = EBNFError;
    fn try_from(pair: Pair<Rule>) -> EBNFResult<SyntacticTerm> {
        let mut pair = pair.into_inner();
        match (pair.next(), pair.next(), pair.next()) {
            (Some(syntactic_factor), Some(except_symbol), Some(syntactic_exception)) => match (
                syntactic_factor.as_rule(),
                except_symbol.as_rule(),
                syntactic_exception.as_rule(),
            ) {
                (Rule::syntactic_factor, Rule::except_symbol, Rule::syntactic_exception) => {
                    Ok(SyntacticTerm {
                        factor: SyntacticFactor::try_from(syntactic_factor)?,
                        // NOTE: If we are this far, failure to obtain a syntactic_exception is not equivalent to it not existing.
                        except: SyntacticException::try_from(syntactic_exception).ok(),
                    })
                }
                (a, b, c) => Err(EBNFError::UnexpectedRules(vec![
                    (a, syntactic_factor.as_str().to_string()),
                    (b, except_symbol.as_str().to_string()),
                    (c, syntactic_exception.as_str().to_string()),
                ])),
            },
            (Some(syntactic_factor), None, None) => Ok(SyntacticTerm {
                factor: SyntacticFactor::try_from(syntactic_factor)?,
                except: None,
            }),
            (a, b, c) => Err(EBNFError::InsufficientTokens(vec![
                a.map(|p| p.as_rule()),
                b.map(|p| p.as_rule()),
                c.map(|p| p.as_rule()),
            ])),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntacticFactor {
    repetition: usize,
    primary: SyntacticPrimary,
}

impl_form_str!(SyntacticFactor, Rule::syntactic_factor);

impl<'r> TryFrom<Pair<'r, Rule>> for SyntacticFactor {
    type Error = EBNFError;
    fn try_from(pair: Pair<Rule>) -> EBNFResult<SyntacticFactor> {
        let mut pair = pair.into_inner();
        match (pair.next(), pair.next(), pair.next()) {
            (Some(integer), Some(repetition_symbol), Some(syntactic_primary)) => match (
                integer.as_rule(),
                repetition_symbol.as_rule(),
                syntactic_primary.as_rule(),
            ) {
                (Rule::integer, Rule::repetition_symbol, Rule::syntactic_primary) => {
                    Ok(SyntacticFactor {
                        repetition: integer.as_str().parse::<usize>()?,
                        primary: SyntacticPrimary::try_from(syntactic_primary)?,
                    })
                }
                (a, b, c) => Err(EBNFError::UnexpectedRules(vec![
                    (a, integer.as_str().to_string()),
                    (b, repetition_symbol.as_str().to_string()),
                    (c, syntactic_primary.as_str().to_string()),
                ])),
            },
            (Some(syntactic_primary), None, None) => match syntactic_primary.as_rule() {
                Rule::syntactic_primary => Ok(SyntacticFactor {
                    repetition: 1,
                    primary: SyntacticPrimary::try_from(syntactic_primary)?,
                }),
                o => Err(EBNFError::UnexpectedRules(vec![(
                    o,
                    syntactic_primary.as_str().to_string(),
                )])),
            },
            (a, b, c) => Err(EBNFError::InsufficientTokens(vec![
                a.map(|p| p.as_rule()),
                b.map(|p| p.as_rule()),
                c.map(|p| p.as_rule()),
            ])),
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum SyntacticPrimary {
    Optional(DefinitionList),
    Repeated(DefinitionList),
    Grouped(DefinitionList),
    // By definition, we have no idea how to parse special sequences.
    // just pass the string to the user.
    Special(String),
    MetaIdentifier(String),
    TerminalString(String),
}

impl_form_str!(SyntacticPrimary, Rule::syntactic_primary);

impl SyntacticPrimary {
    fn parse_definition_list_in_sequence(
        mut pair: Pairs<Rule>,
        sequence_begin_symbol: Rule,
        sequence_end_symbol: Rule,
    ) -> EBNFResult<DefinitionList> {
        match (pair.next(), pair.next(), pair.next()) {
            (Some(start_repeat_symbol), Some(definition_list), Some(end_repeat_symbol)) => {
                match (
                    start_repeat_symbol.as_rule(),
                    definition_list.as_rule(),
                    end_repeat_symbol.as_rule(),
                ) {
                    (begin_symbol, Rule::definition_list, end_symbol) => {
                        if begin_symbol == sequence_begin_symbol
                            && end_symbol == sequence_end_symbol
                        {
                            Ok(DefinitionList::try_from(definition_list)?)
                        } else {
                            Err(EBNFError::UnexpectedRules(vec![
                                (begin_symbol, start_repeat_symbol.as_str().to_string()),
                                (Rule::definition_list, definition_list.as_str().to_string()),
                                (end_symbol, end_repeat_symbol.as_str().to_string()),
                            ]))
                        }
                    }
                    (a, b, c) => Err(EBNFError::UnexpectedRules(vec![
                        (a, start_repeat_symbol.as_str().to_string()),
                        (b, definition_list.as_str().to_string()),
                        (c, end_repeat_symbol.as_str().to_string()),
                    ])),
                }
            }
            (a, b, c) => Err(EBNFError::InsufficientTokens(vec![
                a.map(|p| p.as_rule()),
                b.map(|p| p.as_rule()),
                c.map(|p| p.as_rule()),
            ])),
        }
    }
}

impl<'r> TryFrom<Pair<'r, Rule>> for SyntacticPrimary {
    type Error = EBNFError;
    fn try_from(pair: Pair<Rule>) -> EBNFResult<SyntacticPrimary> {
        let mut pair = pair.into_inner();
        match pair.next() {
            Some(pair) => match pair.as_rule() {
                Rule::optional_sequence => {
                    let pair = pair.into_inner();
                    Ok(SyntacticPrimary::Optional(
                        SyntacticPrimary::parse_definition_list_in_sequence(
                            pair,
                            Rule::start_option_symbol,
                            Rule::end_option_symbol,
                        )?,
                    ))
                }
                Rule::repeated_sequence => {
                    let pair = pair.into_inner();
                    Ok(SyntacticPrimary::Repeated(
                        SyntacticPrimary::parse_definition_list_in_sequence(
                            pair,
                            Rule::start_repeat_symbol,
                            Rule::end_repeat_symbol,
                        )?,
                    ))
                }
                Rule::grouped_sequence => {
                    let pair = pair.into_inner();
                    Ok(SyntacticPrimary::Grouped(
                        SyntacticPrimary::parse_definition_list_in_sequence(
                            pair,
                            Rule::start_group_symbol,
                            Rule::end_group_symbol,
                        )?,
                    ))
                }
                Rule::meta_identifier => Ok(SyntacticPrimary::MetaIdentifier(
                    MetaIdentifier::try_from(pair)?.0,
                )),
                Rule::terminal_string => Ok(SyntacticPrimary::TerminalString(String::from(
                    pair.as_str(),
                ))),
                Rule::special_sequence => {
                    Ok(SyntacticPrimary::Special(String::from(pair.as_str())))
                }
                o => Err(EBNFError::UnexpectedRules(vec![(
                    o,
                    pair.as_str().to_string(),
                )])),
            },
            None => Err(EBNFError::NoTokens),
        }
    }
}

#[test]
fn parse_meta_identifier() {
    let pair = InnerParser::parse(Rule::meta_identifier, r#"letter"#)
        .unwrap()
        .next()
        .unwrap();
    assert_eq!(
        MetaIdentifier::try_from(pair).unwrap(),
        MetaIdentifier("letter".to_string())
    );
}

#[test]
fn parse_symbols() {
    InnerParser::parse(Rule::defining_symbol, r#"="#).unwrap();
    InnerParser::parse(Rule::definition_separator_symbol, r#"|"#).unwrap();
    InnerParser::parse(Rule::first_quote_symbol, r#"'"#).unwrap();
    InnerParser::parse(Rule::second_quote_symbol, r#"""#).unwrap();
    InnerParser::parse(Rule::repetition_symbol, r#"*"#).unwrap();
}

#[test]
fn parse_terminal_string() {
    InnerParser::parse(Rule::terminal_string, r#""b \r a \n d""#).unwrap();
    InnerParser::parse(Rule::terminal_string, r#"'a'"#).unwrap();
}

#[test]
fn parse_syntactic_factor() {
    let pair = InnerParser::parse(Rule::syntactic_factor, r#"5 * {"abcde"}"#)
        .unwrap()
        .next()
        .unwrap();
    assert_eq!(
        SyntacticFactor::try_from(pair).unwrap(),
        SyntacticFactor {
            repetition: 5,
            primary: SyntacticPrimary::Repeated(DefinitionList(vec![SingleDefinition(vec![
                SyntacticTerm {
                    factor: SyntacticFactor {
                        repetition: 1,
                        primary: SyntacticPrimary::TerminalString(r#""abcde""#.to_string())
                    },
                    except: None
                }
            ])]))
        }
    );
}

#[test]
fn parse_syntactic_term() {
    let pair = InnerParser::parse(Rule::syntactic_term, r#"{"abcde"} - "xyz""#)
        .unwrap()
        .next()
        .unwrap();
    assert_eq!(
        SyntacticTerm::try_from(pair).unwrap(),
        SyntacticTerm {
            factor: SyntacticFactor {
                repetition: 1,
                primary: SyntacticPrimary::Repeated(DefinitionList(vec![SingleDefinition(vec![
                    SyntacticTerm {
                        factor: SyntacticFactor {
                            repetition: 1,
                            primary: SyntacticPrimary::TerminalString(r#""abcde""#.to_string())
                        },
                        except: None
                    }
                ])]))
            },
            except: Some(SyntacticException(r#""xyz""#.to_string()))
        }
    );
}

#[test]
fn parse_definition_list() {
    let pair = InnerParser::parse(
        Rule::definition_list,
        r#"(5 * {"abcde"} - "xyz") | "fghi", "ghi";"#,
    )
    .unwrap()
    .next()
    .unwrap();
    assert_eq!(
        DefinitionList::try_from(pair).unwrap(),
        DefinitionList(vec![
            SingleDefinition(vec![SyntacticTerm {
                factor: SyntacticFactor {
                    repetition: 1,
                    primary: SyntacticPrimary::Grouped(DefinitionList(vec![SingleDefinition(
                        vec![SyntacticTerm {
                            factor: SyntacticFactor {
                                repetition: 5,
                                primary: SyntacticPrimary::Repeated(DefinitionList(vec![
                                    SingleDefinition(vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "\"abcde\"".to_string()
                                            )
                                        },
                                        except: None
                                    }])
                                ]))
                            },
                            except: Some(SyntacticException("\"xyz\"".to_string()))
                        }]
                    )]))
                },
                except: None
            }]),
            SingleDefinition(vec![
                SyntacticTerm {
                    factor: SyntacticFactor {
                        repetition: 1,
                        primary: SyntacticPrimary::TerminalString("\"fghi\"".to_string())
                    },
                    except: None
                },
                SyntacticTerm {
                    factor: SyntacticFactor {
                        repetition: 1,
                        primary: SyntacticPrimary::TerminalString("\"ghi\"".to_string())
                    },
                    except: None
                }
            ])
        ])
    );
}

#[test]
fn parse_syntax_rule() {
    let pair = InnerParser::parse(Rule::syntax_rule, r#"letter = "a" | "b";"#)
        .unwrap()
        .next()
        .unwrap();
    assert_eq!(
        SyntaxRule::try_from(pair).unwrap(),
        SyntaxRule {
            identifier: MetaIdentifier("letter".to_string()),
            definitions: DefinitionList::try_from(
                InnerParser::parse(Rule::definition_list, r#""a" | "b""#)
                    .unwrap()
                    .next()
                    .unwrap()
            )
            .unwrap()
        }
    );
    let pair = InnerParser::parse(
        Rule::syntax,
        r#"
        (* comment *) letter (* comment *)
        = (* comment *) "a" (* comment *) | (* comment *) "b" (* comment *) ;"#,
    )
    .unwrap()
    .next()
    .unwrap();
    assert_eq!(
        Syntax::try_from(pair).unwrap(),
        Syntax {
            rules: vec![SyntaxRule {
                identifier: MetaIdentifier("letter".to_string()),
                definitions: DefinitionList::try_from(
                    InnerParser::parse(Rule::definition_list, r#""a" | "b""#)
                        .unwrap()
                        .next()
                        .unwrap()
                )
                .unwrap()
            }]
        }
    );
}

#[test]
fn parse_ebnf_itself() {
    let _=  InnerParser::parse(
            Rule::syntax,
            r#"
            (*
            The syntax of Extended BNF can be defined using
            itself. There are four parts in this example,
            the first part names the characters, the second
            part defines the removal of unnecessary nonprinting characters, the third part defines the
            removal of textual comments, and the final part
            defines the structure of Extended BNF itself.
            Each syntax rule in this example starts with a
            comment that identifies the corresponding clause
            in the standard.
            The meaning of special-sequences is not defined
            in the standard. In this example (see the
            reference to 7.6) they represent control
            functions defined by ISO/IEC 6429:1992.
            Another special-sequence defines a
            syntactic-exception (see the reference to 4.7).
            *)
            (*
            The first part of the lexical syntax defines the
            characters in the 7-bit character set (ISO/IEC
            646:1991) that represent each terminal-character
            and gap-separator in Extended BNF.
            *)
            (* see 7.2 *) 
            letter = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | ’h’
            | ’i’ | ’j’ | ’k’ | ’l’ | ’m’ | ’n’ | ’o’ | ’p’
            | ’q’ | ’r’ | ’s’ | ’t’ | ’u’ | ’v’ | ’w’ | ’x’
            | ’y’ | ’z’
            | ’A’ | ’B’ | ’C’ | ’D’ | ’E’ | ’F’ | ’G’ | ’H’
            | ’I’ | ’J’ | ’K’ | ’L’ | ’M’ | ’N’ | ’O’ | ’P’
            | ’Q’ | ’R’ | ’S’ | ’T’ | ’U’ | ’V’ | ’W’ | ’X’
            | ’Y’ | ’Z’;
            (* see 7.2 *) decimal_digit
            = ’0’ | ’1’ | ’2’ | ’3’ | ’4’ | ’5’ | ’6’ | ’7’
            | ’8’ | ’9’;
            (*
            The representation of the following
            terminal-characters is defined in clauses 7.3,
            7.4 and tables 1, 2.
            *)
            concatenate_symbol = ',';
            defining_symbol = '=';
            definition_separator_symbol = '|' | '//' | '!';
            end_comment_symbol = '*)';
            end_group_symbol = ')';
            end_option_symbol = ']' | '/)';
            end_repeat_symbol = '}' | ':)';
            except_symbol = '-';
            first_quote_symbol = "'";
            repetition_symbol = '*';
            second_quote_symbol = '"';
            special_sequence_symbol = '.unwrap()';
            start_comment_symbol = '(*';
            start_group_symbol = '(';
            start_option_symbol = '[' | '(//';
            start_repeat_symbol = '{' | '(:';
            terminator_symbol = ';' | '.';
            (* see 7.5 *) other_character
            = ' ' | ':' | '+' | '_' | '%' | '@'
            | '&' | '#' | '$' | '<' | '>' | '\'
            | ’ˆ’ | ’‘’ | ’˜’;
            (* see 7.6 *) space_character = ' ';                
            "#,
        ).unwrap().next().unwrap();
}
