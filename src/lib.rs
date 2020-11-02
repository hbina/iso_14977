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
pub struct MetaIdentifier {
    value: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntaxRule {
    identifier: MetaIdentifier,
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
pub struct SyntacticException {
    content: String,
}

#[derive(Debug, Eq, PartialEq)]
pub struct SyntacticTerm {
    factor: SyntacticFactor,
    except: Option<SyntacticException>,
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
    MetaIdentifier(MetaIdentifier),
    TerminalString(String),
}

impl EBNFParser {
    pub fn parse_meta_identifier(pair: Pair<Rule>) -> Option<MetaIdentifier> {
        match pair.as_rule() {
            Rule::meta_identifier => Some(MetaIdentifier {
                value: String::from(pair.as_str()),
            }),
            _ => None,
        }
    }

    pub fn parse_syntax(pair: Pair<Rule>) -> Option<Syntax> {
        let pair = pair.into_inner();
        Some(Syntax {
            rules: pair
                // TODO: We should really check that EOI only exists in the beginning.
                .filter(|rule| rule.as_rule() != Rule::EOI)
                .map(|rule| EBNFParser::parse_syntax_rule(rule))
                .collect::<Option<Vec<SyntaxRule>>>()?,
        })
    }

    pub fn parse_syntax_rule(pair: Pair<Rule>) -> Option<SyntaxRule> {
        let mut pair = pair.into_inner();
        Some(match (pair.next(), pair.next(), pair.next(), pair.next()) {
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
                ) => SyntaxRule {
                    identifier: EBNFParser::parse_meta_identifier(meta_identifier)?,
                    definitions: EBNFParser::parse_definition_list(definition_list)?,
                },
                _ => panic!(
                    r#"
                parse_syntax_rule is unable to match against the required pattern.\n
                Received:\n
                1. {:#?}\n
                2. {:#?}\n
                3. {:#?}\n
                4. {:#?}\n
                rest :{:#?}"#,
                    meta_identifier, defining_symbol, definition_list, terminator_symbol, pair
                ),
            },
            _ => panic!(
                r#"
                parse_syntax_rule is unable to acquire enough pairs.\n
                pairs:{:#?}"#,
                pair
            ),
        })
    }

    pub fn parse_definition_list(pair: Pair<Rule>) -> Option<DefinitionList> {
        let pair = pair.into_inner();
        Some(DefinitionList {
            list: pair
                .step_by(2)
                .map(|definition_list| EBNFParser::parse_single_definition(definition_list))
                .collect::<Option<Vec<SingleDefinition>>>()?,
        })
    }

    pub fn parse_single_definition(pair: Pair<Rule>) -> Option<SingleDefinition> {
        let pair = pair.into_inner();
        Some(SingleDefinition {
            terms: pair
                .step_by(2)
                .map(|syntactic_term| EBNFParser::parse_syntactic_term(syntactic_term))
                .collect::<Option<Vec<SyntacticTerm>>>()?,
        })
    }

    pub fn parse_syntactic_exception(pair: Pair<Rule>) -> Option<SyntacticException> {
        let pair = pair.into_inner();
        Some(SyntacticException {
            content: String::from(pair.as_str()),
        })
    }

    pub fn parse_syntactic_term(pair: Pair<Rule>) -> Option<SyntacticTerm> {
        let mut pair = pair.into_inner();
        match (pair.next(), pair.next(), pair.next()) {
            (Some(syntactic_factor), Some(except_symbol), Some(syntactic_exception)) => match (
                syntactic_factor.as_rule(),
                except_symbol.as_rule(),
                syntactic_exception.as_rule(),
            ) {
                (Rule::syntactic_factor, Rule::except_symbol, Rule::syntactic_exception) => {
                    Some(SyntacticTerm {
                        factor: EBNFParser::parse_syntactic_factor(syntactic_factor)?,
                        // NOTE: If we are this far, failure to obtain a syntactic_exception is not equivalent to it not existing.
                        except: Some(EBNFParser::parse_syntactic_exception(syntactic_exception)?),
                    })
                }
                _ => panic!(
                    r#"
parse_syntactic_term is unable to match against the required pattern.
Received:
1. {:#?}
2. {:#?}
3. {:#?}
rest.\n{:#?}
                    "#,
                    syntactic_factor, except_symbol, syntactic_exception, pair
                ),
            },
            (Some(syntactic_factor), None, None) => Some(SyntacticTerm {
                factor: EBNFParser::parse_syntactic_factor(syntactic_factor)?,
                except: None,
            }),
            triplets => panic!(
                r#"
parse_syntax_rule is unable to acquire enough pairs.
triplets:
    {:#?}
pairs:
    {:#?}
                "#,
                triplets, pair
            ),
        }
    }

    pub fn parse_syntactic_factor(pair: Pair<Rule>) -> Option<SyntacticFactor> {
        let mut pair = pair.into_inner();
        match (pair.next(), pair.next(), pair.next()) {
            (Some(integer), Some(repetition_symbol), Some(syntactic_primary)) => match (
                integer.as_rule(),
                repetition_symbol.as_rule(),
                syntactic_primary.as_rule(),
            ) {
                (Rule::integer, Rule::repetition_symbol, Rule::syntactic_primary) => {
                    Some(SyntacticFactor {
                        repetition: integer
                            .as_str()
                            .parse::<usize>()
                            .expect("Unable to parse integer required for syntactic_factor."),
                        primary: EBNFParser::parse_syntactic_primary(syntactic_primary)?,
                    })
                }
                _ => panic!("parse_syntactic_factor is unable to match with the pattern (integer, repetition_symbol, syntactic_primary)"),
            },
            (Some(syntactic_primary), None, None) => match syntactic_primary.as_rule() {
                Rule::syntactic_primary => Some(SyntacticFactor {
                    repetition: 1,
                    primary: EBNFParser::parse_syntactic_primary(syntactic_primary)?,
                }),
                rule => panic!("expected Rule::syntactic_primary, instead we got {:#?}", rule),
            },
            triplet => panic!(r#"
            Invalid sequence of pairs given to parse_syntactic_factor.\n
            triplet:\n{:#?}\n
            rest:\n{:#?}\n"#, triplet, pair),
        }
    }

    fn parse_definition_list_in_sequence(
        mut pair: Pairs<Rule>,
        sequence_begin_symbol: Rule,
        sequence_end_symbol: Rule,
    ) -> Option<DefinitionList> {
        match (pair.next(), pair.next(), pair.next()) {
            (Some(start_repeat_symbol), Some(definition_list), Some(end_repeat_symbol)) => {
                match (
                    start_repeat_symbol.as_rule(),
                    definition_list.as_rule(),
                    end_repeat_symbol.as_rule(),
                ) {
                    // NOTE: Why does this become a binding?
                    (begin_symbol, Rule::definition_list, end_symbol) => {
                        if begin_symbol == sequence_begin_symbol
                            && end_symbol == sequence_end_symbol
                        {
                            Some(EBNFParser::parse_definition_list(definition_list)?)
                        } else {
                            None
                        }
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn parse_syntactic_primary(pair: Pair<Rule>) -> Option<SyntacticPrimary> {
        let pair = pair.into_inner().next()?;
        Some(match pair.as_rule() {
            Rule::optional_sequence => {
                let pair = pair.into_inner();
                SyntacticPrimary::Optional(EBNFParser::parse_definition_list_in_sequence(
                    pair,
                    Rule::start_option_symbol,
                    Rule::end_option_symbol,
                )?)
            }
            Rule::repeated_sequence => {
                let pair = pair.into_inner();
                SyntacticPrimary::Repeated(EBNFParser::parse_definition_list_in_sequence(
                    pair,
                    Rule::start_repeat_symbol,
                    Rule::end_repeat_symbol,
                )?)
            }
            Rule::grouped_sequence => {
                let pair = pair.into_inner();
                SyntacticPrimary::Grouped(EBNFParser::parse_definition_list_in_sequence(
                    pair,
                    Rule::start_group_symbol,
                    Rule::start_group_symbol,
                )?)
            }
            Rule::meta_identifier => {
                SyntacticPrimary::MetaIdentifier(EBNFParser::parse_meta_identifier(pair)?)
            }
            Rule::terminal_string => SyntacticPrimary::TerminalString(String::from(pair.as_str())),
            Rule::special_sequence => SyntacticPrimary::Special(String::from(pair.as_str())),
            _ => panic!(
                r#"
            parse_syntactic_primary is unable to match any of the expected enum.
            Instead, we got {:#?}"#,
                pair
            ),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use pest::Parser;

    #[test]
    fn parse_meta_identifier() -> Result<(), Box<dyn std::error::Error>> {
        if let Some(pair) = EBNFParser::parse(Rule::meta_identifier, r#"letter"#)?.next() {
            assert_eq!(
                EBNFParser::parse_meta_identifier(pair),
                Some(MetaIdentifier {
                    value: "letter".to_string()
                })
            );
        };

        Ok(())
    }

    #[test]
    fn parse_symbols() -> Result<(), Box<dyn std::error::Error>> {
        EBNFParser::parse(Rule::defining_symbol, r#"="#)?;
        EBNFParser::parse(Rule::definition_separator_symbol, r#"|"#)?;
        EBNFParser::parse(Rule::first_quote_symbol, r#"’"#)?;
        EBNFParser::parse(Rule::second_quote_symbol, r#"""#)?;
        EBNFParser::parse(Rule::repetition_symbol, r#"*"#)?;
        Ok(())
    }

    #[test]
    fn parse_terminal_string() -> Result<(), Box<dyn std::error::Error>> {
        EBNFParser::parse(Rule::terminal_string, r#""b \r a \n d""#)?;
        EBNFParser::parse(Rule::terminal_string, r#"’a’"#)?;
        Ok(())
    }

    #[test]
    fn parse_syntactic_factor() -> Result<(), Box<dyn std::error::Error>> {
        if let Some(pair) = EBNFParser::parse(Rule::syntactic_factor, r#"5 * {"abcde"}"#)?.next() {
            assert_eq!(
                EBNFParser::parse_syntactic_factor(pair),
                Some(SyntacticFactor {
                    repetition: 5,
                    primary: SyntacticPrimary::Repeated(DefinitionList {
                        list: vec![SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    repetition: 1,
                                    primary: SyntacticPrimary::TerminalString(
                                        r#""abcde""#.to_string()
                                    )
                                },
                                except: None
                            }]
                        }]
                    })
                })
            )
        };
        Ok(())
    }

    #[test]
    fn parse_syntactic_term() -> Result<(), Box<dyn std::error::Error>> {
        if let Some(pair) = EBNFParser::parse(Rule::syntactic_term, r#"{"abcde"} - "xyz""#)?.next()
        {
            assert_eq!(
                EBNFParser::parse_syntactic_term(pair),
                Some(SyntacticTerm {
                    factor: SyntacticFactor {
                        repetition: 1,
                        primary: SyntacticPrimary::Repeated(DefinitionList {
                            list: vec![SingleDefinition {
                                terms: vec![SyntacticTerm {
                                    factor: SyntacticFactor {
                                        repetition: 1,
                                        primary: SyntacticPrimary::TerminalString(
                                            r#""abcde""#.to_string()
                                        )
                                    },
                                    except: None
                                }]
                            }]
                        })
                    },
                    except: Some(SyntacticException {
                        content: r#""xyz""#.to_string()
                    })
                })
            )
        };
        Ok(())
    }

    #[test]
    fn parse_definition_list() -> Result<(), Box<dyn std::error::Error>> {
        if let Some(pair) = EBNFParser::parse(
            Rule::definition_list,
            r#"(5 * {"abcde"} - "xyz") | "fghi", "ghi";"#,
        )?
        .next()
        {
            EBNFParser::parse_definition_list(pair);
        };
        Ok(())
    }

    #[test]
    fn parse_syntax_rule() -> Result<(), Box<dyn std::error::Error>> {
        if let Some(pair) = EBNFParser::parse(Rule::syntax_rule, r#"letter = "a" | "b";"#)?.next() {
            assert_eq!(
                EBNFParser::parse_syntax_rule(pair),
                Some(SyntaxRule {
                    identifier: MetaIdentifier {
                        value: "letter".to_string()
                    },
                    definitions: EBNFParser::parse_definition_list(
                        EBNFParser::parse(Rule::definition_list, r#""a" | "b""#)?
                            .next()
                            .unwrap()
                    )
                    .unwrap()
                })
            );
        };
        if let Some(pair) = EBNFParser::parse(
            Rule::syntax,
            r#"
        (* comment *) letter (* comment *)
        = (* comment *) "a" (* comment *) | (* comment *) "b" (* comment *) ;"#,
        )?
        .next()
        {
            assert_eq!(
                EBNFParser::parse_syntax(pair),
                Some(Syntax {
                    rules: vec![SyntaxRule {
                        identifier: MetaIdentifier {
                            value: "letter".to_string()
                        },
                        definitions: EBNFParser::parse_definition_list(
                            EBNFParser::parse(Rule::definition_list, r#""a" | "b""#)?
                                .next()
                                .unwrap()
                        )
                        .unwrap()
                    }]
                })
            );
        };
        Ok(())
    }

    #[test]
    fn parse_ebnf_itself() -> Result<(), Box<dyn std::error::Error>> {
        if let Some(_) =  EBNFParser::parse(
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
            letter = ’a’ | ’b’ | ’c’ | ’d’ | ’e’ | ’f’ | ’g’ | ’h’
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
            concatenate_symbol = ’,’;
            defining_symbol = ’=’;
            definition_separator_symbol = ’|’ | ’//’ | ’!’;
            end_comment_symbol = ’*)’;
            end_group_symbol = ’)’;
            end_option_symbol = ’]’ | ’/)’;
            end_repeat_symbol = ’}’ | ’:)’;
            except_symbol = ’-’;
            first_quote_symbol = "’";
            repetition_symbol = ’*’;
            second_quote_symbol = ’"’;
            special_sequence_symbol = ’?’;
            start_comment_symbol = ’(*’;
            start_group_symbol = ’(’;
            start_option_symbol = ’[’ | ’(//’;
            start_repeat_symbol = ’{’ | ’(:’;
            terminator_symbol = ’;’ | ’.’;
            (* see 7.5 *) other_character
            = ’ ’ | ’:’ | ’+’ | ’_’ | ’%’ | ’@’
            | ’&’ | ’#’ | ’$’ | ’<’ | ’>’ | ’\’
            | ’ˆ’ | ’‘’ | ’˜’;
            (* see 7.6 *) space_character = ’ ’;                
            "#,
        )?.next() {
            // TODO: Complete this :)
            // assert_eq!(EBNFParser::parse_syntax(pair), None);
        };
        Ok(())
    }
}
