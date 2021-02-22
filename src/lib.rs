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
    pub fn parse_meta_identifier(pair: Pair<Rule>) -> Option<String> {
        match pair.as_rule() {
            Rule::meta_identifier => Some(pair.as_str().to_string()),
            _ => None,
        }
    }

    pub fn parse_syntax(pair: Pair<Rule>) -> Option<Syntax> {
        let pair = pair.into_inner();
        Some(Syntax {
            rules: pair
                // TODO: We should really check that EOI only exists in the beginning.
                .filter(|rule| rule.as_rule() != Rule::EOI)
                .map(EBNFParser::parse_syntax_rule)
                .collect::<Option<Vec<SyntaxRule>>>()?,
        })
    }

    pub fn parse_syntax_rule(pair: Pair<Rule>) -> Option<SyntaxRule> {
        let mut pair = pair.into_inner();
        if pair.clone().map(|p| p.as_rule()).collect::<Vec<_>>()
            == vec![
                Rule::meta_identifier,
                Rule::defining_symbol,
                Rule::definition_list,
                Rule::terminator_symbol,
            ]
        {
            let meta_identifier = pair.next().unwrap();
            let _defining_symbol = pair.next().unwrap();
            let definition_list = pair.next().unwrap();

            Some(SyntaxRule {
                identifier: EBNFParser::parse_meta_identifier(meta_identifier)?,
                definitions: EBNFParser::parse_definition_list(definition_list)?,
            })
        } else {
            None
        }
    }

    pub fn parse_definition_list(pair: Pair<Rule>) -> Option<DefinitionList> {
        let pair = pair.into_inner();
        Some(DefinitionList {
            list: pair
                // skipping over definition_separator_symbol's
                .step_by(2)
                .map(EBNFParser::parse_single_definition)
                .collect::<Option<Vec<SingleDefinition>>>()?,
        })
    }

    pub fn parse_single_definition(pair: Pair<Rule>) -> Option<SingleDefinition> {
        let pair = pair.into_inner();
        Some(SingleDefinition {
            terms: pair
                // skipping over concatenate_symbol's
                .step_by(2)
                .map(EBNFParser::parse_syntactic_term)
                .collect::<Option<Vec<SyntacticTerm>>>()?,
        })
    }

    pub fn parse_syntactic_exception(pair: Pair<Rule>) -> Option<SyntacticFactor> {
        let pair = pair.into_inner().next().unwrap();
        let factor = Self::parse_syntactic_factor(pair)?;
        // FIXME: check restriction from § 4.7:
        // factor must be non-recursive
        Some(factor)
    }

    pub fn parse_syntactic_term(pair: Pair<Rule>) -> Option<SyntacticTerm> {
        let mut pair = pair.into_inner();

        let factor;
        let mut except = None;

        let rules = pair.clone().map(|p| p.as_rule()).collect::<Vec<_>>();
        if rules
            == vec![
                Rule::syntactic_factor,
                Rule::except_symbol,
                Rule::syntactic_exception,
            ]
        {
            let syntactic_factor = pair.next().unwrap();
            let _except_symbol = pair.next().unwrap();
            let syntactic_exception = pair.next().unwrap();

            factor = EBNFParser::parse_syntactic_factor(syntactic_factor)?;
            // NOTE: If we are this far, failure to obtain a syntactic_exception is not equivalent to it not existing.
            except = Some(EBNFParser::parse_syntactic_exception(syntactic_exception)?)
        } else if rules == vec![Rule::syntactic_factor] {
            factor = EBNFParser::parse_syntactic_factor(pair.next().unwrap())?;
        } else {
            return None;
        }

        Some(SyntacticTerm { factor, except })
    }

    pub fn parse_syntactic_factor(pair: Pair<Rule>) -> Option<SyntacticFactor> {
        let mut pair = pair.into_inner();

        let rules = pair.clone().map(|p| p.as_rule()).collect::<Vec<_>>();

        let mut repetition = 1;

        match rules.as_slice() {
            [Rule::integer, Rule::repetition_symbol, Rule::syntactic_primary] => {
                repetition = pair
                    .next()
                    .unwrap()
                    .as_str()
                    .parse::<usize>()
                    .expect("Unable to parse integer required for syntactic_factor.");
                let _repetition_symbol = pair.next();
            }
            [Rule::syntactic_primary] => {}
            _ => return None,
        };

        let primary = EBNFParser::parse_syntactic_primary(pair.next().unwrap()).unwrap();

        Some(SyntacticFactor {
            repetition,
            primary,
        })
    }

    fn parse_definition_list_in_sequence(
        pair: Pairs<Rule>,
        sequence_begin_symbol: Rule,
        sequence_end_symbol: Rule,
    ) -> Option<DefinitionList> {
        if pair.clone().map(|p| p.as_rule()).collect::<Vec<_>>()
            == vec![
                sequence_begin_symbol,
                Rule::definition_list,
                sequence_end_symbol,
            ]
        {
            let definition_list = pair.skip(1).next().unwrap();
            Some(EBNFParser::parse_definition_list(definition_list)?)
        } else {
            None
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
                    Rule::end_group_symbol,
                )?)
            }
            Rule::meta_identifier => {
                SyntacticPrimary::MetaIdentifier(EBNFParser::parse_meta_identifier(pair)?)
            }
            Rule::terminal_string => SyntacticPrimary::TerminalString(extract_content(pair)),
            Rule::special_sequence => SyntacticPrimary::Special(extract_content(pair)),
            Rule::empty_sequence => SyntacticPrimary::Empty,
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
                Some("letter".to_string())
            );
        };

        Ok(())
    }

    #[test]
    fn parse_symbols() -> Result<(), Box<dyn std::error::Error>> {
        EBNFParser::parse(Rule::defining_symbol, "=")?;
        EBNFParser::parse(Rule::definition_separator_symbol, "|")?;
        EBNFParser::parse(Rule::first_quote_symbol, "'")?;
        EBNFParser::parse(Rule::second_quote_symbol, r#"""#)?;
        EBNFParser::parse(Rule::repetition_symbol, "*")?;
        Ok(())
    }

    #[test]
    fn parse_terminal_string() -> Result<(), Box<dyn std::error::Error>> {
        EBNFParser::parse(Rule::terminal_string, r#""b \r a \n d""#)?;
        EBNFParser::parse(Rule::terminal_string, "'a'")?;
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
                                    primary: SyntacticPrimary::TerminalString("abcde".to_string())
                                },
                                except: None
                            }]
                        }]
                    })
                })
            )
        }
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
                                            "abcde".to_string()
                                        )
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
                })
            )
        }
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
        }
        Ok(())
    }

    #[test]
    fn parse_syntax_rule() -> Result<(), Box<dyn std::error::Error>> {
        if let Some(pair) = EBNFParser::parse(Rule::syntax_rule, r#"letter = "a" | "b";"#)?.next() {
            assert_eq!(
                EBNFParser::parse_syntax_rule(pair),
                Some(SyntaxRule {
                    identifier: "letter".to_string(),
                    definitions: EBNFParser::parse_definition_list(
                        EBNFParser::parse(Rule::definition_list, r#""a" | "b""#)?
                            .next()
                            .unwrap()
                    )
                    .unwrap()
                })
            );
        }
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
                        identifier: "letter".to_string(),
                        definitions: EBNFParser::parse_definition_list(
                            EBNFParser::parse(Rule::definition_list, r#""a" | "b""#)?
                                .next()
                                .unwrap()
                        )
                        .unwrap()
                    }]
                })
            );
        }
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
            | 'ˆ' | '‘' | '˜';
            (* see 7.6 *) space_character = ' ';
            "#,
        )?.next() {
            // TODO: Complete this :)
            // assert_eq!(EBNFParser::parse_syntax(pair), None);
        }
        Ok(())
    }
}
