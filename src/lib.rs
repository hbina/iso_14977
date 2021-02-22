#[macro_use]
extern crate pest_derive;

use pest::iterators::{Pair, Pairs};

#[derive(Parser)]
#[grammar = "ebnf.pest"]
pub struct EBNFParser;

#[derive(Debug, Eq, PartialEq)]
pub struct Syntax {
    rules: Vec<SyntaxRule>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntaxRule {
    identifier: String,
    definitions: DefinitionList,
}

#[derive(Debug, Eq, PartialEq)]
pub struct DefinitionList {
    list: Vec<SingleDefinition>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct SingleDefinition {
    terms: Vec<SyntacticTerm>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntacticTerm {
    factor: SyntacticFactor,
    except: Option<SyntacticFactor>,
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntacticFactor {
    repetition: usize,
    primary: SyntacticPrimary,
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
    Empty,
}

fn extract_content(pair: Pair<Rule>) -> String {
    // remove the first and last character
    let mut chars = pair.as_str().chars();
    chars.next();
    chars.next_back();
    chars.collect::<String>()
}

impl EBNFParser {
    pub fn parse_meta_identifier(pair: Pair<Rule>) -> String {
        assert_eq!(pair.as_rule(), Rule::meta_identifier);
        pair.as_str().to_string()
    }

    pub fn parse_syntax(pair: Pair<Rule>) -> Syntax {
        assert_eq!(pair.as_rule(), Rule::syntax);

        Syntax {
            rules: pair
                .into_inner()
                .filter(|p| p.as_rule() != Rule::EOI)
                .map(EBNFParser::parse_syntax_rule)
                .collect::<Vec<SyntaxRule>>(),
        }
    }

    pub fn parse_syntax_rule(pair: Pair<Rule>) -> SyntaxRule {
        assert_eq!(pair.as_rule(), Rule::syntax_rule);

        let mut pairs = pair.into_inner();

        let meta_identifier = pairs.next().unwrap();
        let _defining_symbol = pairs.next();
        let definition_list = pairs.next().unwrap();
        let _terminator_symbol = pairs.next();

        SyntaxRule {
            identifier: EBNFParser::parse_meta_identifier(meta_identifier),
            definitions: EBNFParser::parse_definition_list(definition_list),
        }
    }

    pub fn parse_definition_list(pair: Pair<Rule>) -> DefinitionList {
        assert_eq!(pair.as_rule(), Rule::definition_list);

        DefinitionList {
            list: pair
                .into_inner()
                // skipping over definition_separator_symbol's
                .step_by(2)
                .map(EBNFParser::parse_single_definition)
                .collect::<Vec<SingleDefinition>>(),
        }
    }

    pub fn parse_single_definition(pair: Pair<Rule>) -> SingleDefinition {
        assert_eq!(pair.as_rule(), Rule::single_definition);

        SingleDefinition {
            terms: pair
                .into_inner()
                // skipping over concatenate_symbol's
                .step_by(2)
                .map(EBNFParser::parse_syntactic_term)
                .collect::<Vec<SyntacticTerm>>(),
        }
    }

    pub fn parse_syntactic_exception(pair: Pair<Rule>) -> SyntacticFactor {
        assert_eq!(pair.as_rule(), Rule::syntactic_exception);

        let pair = pair.into_inner().next().unwrap();
        let factor = Self::parse_syntactic_factor(pair);
        // FIXME: check restriction from § 4.7:
        // factor must be non-recursive
        factor
    }

    pub fn parse_syntactic_term(pair: Pair<Rule>) -> SyntacticTerm {
        assert_eq!(pair.as_rule(), Rule::syntactic_term);

        let mut pairs = pair.into_inner();
        let factor = EBNFParser::parse_syntactic_factor(pairs.next().unwrap());

        let _except_symbol = pairs.next();
        let except = pairs.next().map(EBNFParser::parse_syntactic_exception);

        SyntacticTerm { factor, except }
    }

    pub fn parse_syntactic_factor(pair: Pair<Rule>) -> SyntacticFactor {
        assert_eq!(pair.as_rule(), Rule::syntactic_factor);

        let mut pairs = pair.into_inner().peekable();

        let repetition = if pairs.peek().unwrap().as_rule() == Rule::integer {
            let temp = pairs
                .next()
                .unwrap()
                .as_str()
                .parse::<usize>()
                .expect("Unable to parse integer required for syntactic_factor.");
            // still have to skip over the repetition symbol
            let _repetition_symbol = pairs.next();
            temp
        } else {
            1
        };

        let primary = EBNFParser::parse_syntactic_primary(pairs.next().unwrap());

        SyntacticFactor {
            repetition,
            primary,
        }
    }

    fn parse_definition_list_in_sequence(pair: Pairs<Rule>) -> DefinitionList {
        let definition_list = pair.skip(1).next().unwrap();
        assert_eq!(definition_list.as_rule(), Rule::definition_list);

        EBNFParser::parse_definition_list(definition_list)
    }

    pub fn parse_syntactic_primary(pair: Pair<Rule>) -> SyntacticPrimary {
        assert_eq!(pair.as_rule(), Rule::syntactic_primary);

        let pair = pair.into_inner().next().unwrap();

        match pair.as_rule() {
            Rule::optional_sequence => SyntacticPrimary::Optional(
                EBNFParser::parse_definition_list_in_sequence(pair.into_inner()),
            ),
            Rule::repeated_sequence => SyntacticPrimary::Repeated(
                EBNFParser::parse_definition_list_in_sequence(pair.into_inner()),
            ),
            Rule::grouped_sequence => SyntacticPrimary::Grouped(
                EBNFParser::parse_definition_list_in_sequence(pair.into_inner()),
            ),
            Rule::meta_identifier => {
                SyntacticPrimary::MetaIdentifier(EBNFParser::parse_meta_identifier(pair))
            }
            Rule::terminal_string => SyntacticPrimary::TerminalString(extract_content(pair)),
            Rule::special_sequence => SyntacticPrimary::Special(extract_content(pair)),
            Rule::empty_sequence => SyntacticPrimary::Empty,
            _ => unreachable!("{:#?}", pair),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pest::Parser;

    #[test]
    fn parse_meta_identifier() {
        assert_eq!(
            EBNFParser::parse(Rule::meta_identifier, r#"letter"#)
                .unwrap()
                .next()
                .map(EBNFParser::parse_meta_identifier)
                .unwrap(),
            "letter".to_string()
        );
    }

    #[test]
    fn parse_symbols() {
        assert!(EBNFParser::parse(Rule::defining_symbol, "=").is_ok());
        assert!(EBNFParser::parse(Rule::definition_separator_symbol, "|").is_ok());
        assert!(EBNFParser::parse(Rule::first_quote_symbol, "'").is_ok());
        assert!(EBNFParser::parse(Rule::second_quote_symbol, r#"""#).is_ok());
        assert!(EBNFParser::parse(Rule::repetition_symbol, "*").is_ok());
    }

    #[test]
    fn parse_terminal_string() {
        assert!(EBNFParser::parse(Rule::terminal_string, r#""b \r a \n d""#).is_ok());
        assert!(EBNFParser::parse(Rule::terminal_string, "'a'").is_ok());
    }

    #[test]
    fn parse_syntactic_factor() {
        assert_eq!(
            EBNFParser::parse(Rule::syntactic_factor, r#"5 * {"abcde"}"#)
                .unwrap()
                .next()
                .map(EBNFParser::parse_syntactic_factor)
                .unwrap(),
            SyntacticFactor {
                repetition: 5,
                primary: SyntacticPrimary::Repeated(DefinitionList {
                    list: vec![SingleDefinition {
                        terms: vec![SyntacticTerm {
                            factor: SyntacticFactor {
                                repetition: 1,
                                primary: SyntacticPrimary::TerminalString("abcde".to_string())
                            },
                            except: None
                        }]
                    }]
                })
            }
        );
    }

    #[test]
    fn parse_syntactic_term() {
        assert_eq!(
            EBNFParser::parse(Rule::syntactic_term, r#"{"abcde"} - "xyz""#)
                .unwrap()
                .next()
                .map(EBNFParser::parse_syntactic_term)
                .unwrap(),
            SyntacticTerm {
                factor: SyntacticFactor {
                    repetition: 1,
                    primary: SyntacticPrimary::Repeated(DefinitionList {
                        list: vec![SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    repetition: 1,
                                    primary: SyntacticPrimary::TerminalString("abcde".to_string())
                                },
                                except: None
                            }]
                        }]
                    })
                },
                except: Some(SyntacticFactor {
                    repetition: 1,
                    primary: SyntacticPrimary::TerminalString("xyz".to_string()),
                })
            }
        );
    }

    #[test]
    fn parse_definition_list() {
        assert_eq!(
            EBNFParser::parse(
                Rule::definition_list,
                r#"(5 * {"abcde"} - "xyz") | "fghi", "ghi";"#
            )
            .unwrap()
            .next()
            .map(EBNFParser::parse_definition_list)
            .unwrap(),
            DefinitionList {
                list: vec![
                    SingleDefinition {
                        terms: vec![SyntacticTerm {
                            factor: SyntacticFactor {
                                repetition: 1,
                                primary: SyntacticPrimary::Grouped(DefinitionList {
                                    list: vec![SingleDefinition {
                                        terms: vec![SyntacticTerm {
                                            factor: SyntacticFactor {
                                                repetition: 5,
                                                primary: SyntacticPrimary::Repeated(DefinitionList {
                                                    list: vec![SingleDefinition {
                                                        terms: vec![SyntacticTerm {
                                                            factor: SyntacticFactor {
                                                                repetition: 1,
                                                                primary:
                                                                    SyntacticPrimary::TerminalString(
                                                                        "abcde".to_string()
                                                                    ),
                                                            },
                                                            except: None,
                                                        },],
                                                    },],
                                                }),
                                            },
                                            except: Some(SyntacticFactor {
                                                repetition: 1,
                                                primary: SyntacticPrimary::TerminalString("xyz".to_string()),
                                            })
                                        }]
                                    }]
                                })
                            },
                            except: None,
                        }]
                    },
                    SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    repetition: 1,
                                    primary: SyntacticPrimary::TerminalString("fghi".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    repetition: 1,
                                    primary: SyntacticPrimary::TerminalString("ghi".to_string()),
                                },
                                except: None,
                            },
                        ],
                    },
                ],
            }
        );
    }

    #[test]
    fn parse_syntax_rule() {
        assert_eq!(
            EBNFParser::parse(Rule::syntax_rule, r#"letter = "a" | "b";"#)
                .unwrap()
                .next()
                .map(EBNFParser::parse_syntax_rule)
                .unwrap(),
            SyntaxRule {
                identifier: "letter".to_string(),
                definitions: DefinitionList {
                    list: vec![
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    repetition: 1,
                                    primary: SyntacticPrimary::TerminalString("a".to_string())
                                },
                                except: None
                            }]
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    repetition: 1,
                                    primary: SyntacticPrimary::TerminalString("b".to_string())
                                },
                                except: None
                            }]
                        }
                    ]
                }
            }
        );
        assert_eq!(
            EBNFParser::parse(
                Rule::syntax,
                r#"(* comment *) letter (* comment *)
= (* comment *) "a" (* comment *) | (* comment *) "b" (* comment *) ;"#
            )
            .unwrap()
            .next()
            .map(EBNFParser::parse_syntax)
            .unwrap(),
            Syntax {
                rules: vec![SyntaxRule {
                    identifier: "letter".to_string(),
                    definitions: DefinitionList {
                        list: vec![
                            SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("a".to_string())
                                    },
                                    except: None
                                }]
                            },
                            SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("b".to_string())
                                    },
                                    except: None
                                }]
                            }
                        ]
                    }
                }]
            }
        );
    }

    #[test]
    fn parse_ebnf_itself() {
        assert_eq!(
            EBNFParser::parse(
                Rule::syntax,
                r#"(*
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
letter = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h'
| 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p'
| 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x'
| 'y' | 'z'
| 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H'
| 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P'
| 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X'
| 'Y' | 'Z';
(* see 7.2 *) decimal_digit
= '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7'
| '8' | '9';
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
special_sequence_symbol = '?';
start_comment_symbol = '(*';
start_group_symbol = '(';
start_option_symbol = '[' | '(//';
start_repeat_symbol = '{' | '(:';
terminator_symbol = ';' | '.';
(* see 7.5 *) other_character
= ' ' | ':' | '+' | '_' | '%' | '@'
| '&' | '#' | '$' | '<' | '>' | '\'
| 'ˆ' | '‘' | '~';
(* see 7.6 *) space_character = ' ';
"#,
            )
            .unwrap()
            .next()
            .map(EBNFParser::parse_syntax)
            .unwrap(),
            Syntax {
                rules: vec![
                    SyntaxRule {
                        identifier: "letter".to_string(),
                        definitions: DefinitionList {
                            list: vec![
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "a".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "b".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "c".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "d".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "e".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "f".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "g".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "h".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "i".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "j".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "k".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "l".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "m".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "n".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "o".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "p".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "q".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "r".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "s".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "t".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "u".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "v".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "w".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "x".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "y".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "z".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "A".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "B".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "C".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "D".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "E".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "F".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "G".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "H".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "I".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "J".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "K".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "L".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "M".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "N".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "O".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "P".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "Q".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "R".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "S".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "T".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "U".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "V".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "W".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "X".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "Y".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "Z".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                }
                            ]
                        }
                    },
                    SyntaxRule {
                        identifier: "decimal_digit".to_string(),
                        definitions: DefinitionList {
                            list: vec![
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "0".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "1".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "2".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "3".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "4".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "5".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "6".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "7".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "8".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "9".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                }
                            ]
                        }
                    },
                    SyntaxRule {
                        identifier: "concatenate_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString(",".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "defining_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("=".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "definition_separator_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "|".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "//".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "!".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                }
                            ]
                        }
                    },
                    SyntaxRule {
                        identifier: "end_comment_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("*)".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "end_group_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString(")".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "end_option_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "]".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "/)".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                }
                            ]
                        }
                    },
                    SyntaxRule {
                        identifier: "end_repeat_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "}".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                ":)".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                }
                            ]
                        }
                    },
                    SyntaxRule {
                        identifier: "except_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("-".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "first_quote_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("\'".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "repetition_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("*".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "second_quote_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("\"".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "special_sequence_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("?".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "start_comment_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("(*".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "start_group_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString("(".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    },
                    SyntaxRule {
                        identifier: "start_option_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "[".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "(//".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                }
                            ]
                        }
                    },
                    SyntaxRule {
                        identifier: "start_repeat_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "{".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "(:".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                }
                            ]
                        }
                    },
                    SyntaxRule {
                        identifier: "terminator_symbol".to_string(),
                        definitions: DefinitionList {
                            list: vec![
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                ";".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                ".".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                }
                            ]
                        }
                    },
                    SyntaxRule {
                        identifier: "other_character".to_string(),
                        definitions: DefinitionList {
                            list: vec![
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                " ".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                ":".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "+".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "_".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "%".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "@".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "&".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "#".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "$".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "<".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                ">".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "\\".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "ˆ".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "‘".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                },
                                SingleDefinition {
                                    terms: vec![SyntacticTerm {
                                        factor: SyntacticFactor {
                                            repetition: 1,
                                            primary: SyntacticPrimary::TerminalString(
                                                "~".to_string()
                                            )
                                        },
                                        except: None
                                    }]
                                }
                            ]
                        }
                    },
                    SyntaxRule {
                        identifier: "space_character".to_string(),
                        definitions: DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString(" ".to_string())
                                    },
                                    except: None
                                }]
                            }]
                        }
                    }
                ]
            },
        );
    }
}
