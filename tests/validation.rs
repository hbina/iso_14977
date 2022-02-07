use iso_14977::{
    is_bracketed_textual_comment, is_comment_symbol, is_commentless_syntax, is_ebnf_syntax,
    is_gap_free_symbol, is_printable_syntax, is_syntactic_primary, is_terminal_string,
    DefinitionList, EbnfSyntax, SingleDefinition, SyntacticFactor, SyntacticPrimary, SyntacticTerm,
    SyntaxRule,
};
use nom::multi::many0;

#[test]
fn test_output_of_gap_free_symbol() {
    assert_eq!(
        is_gap_free_symbol("'hello'"),
        Ok(("", "'hello'".to_string()))
    );
    assert_eq!(
        is_gap_free_symbol("\"hello\""),
        Ok(("", "\"hello\"".to_string()))
    );
    assert_eq!(is_gap_free_symbol("a"), Ok(("", "a".to_string())));
    assert_eq!(is_gap_free_symbol("ab"), Ok(("b", "a".to_string())));
    assert_eq!(
        is_gap_free_symbol("hello world"),
        Ok(("ello world", "h".to_string()))
    );
    assert_eq!(
        is_gap_free_symbol("'hello world'"),
        Ok(("", "'hello world'".to_string()))
    );
    assert_eq!(
        is_gap_free_symbol("\"hello world\""),
        Ok(("", "\"hello world\"".to_string()))
    );
    assert_eq!(
        is_gap_free_symbol("'hello' world"),
        Ok((" world", "'hello'".to_string()))
    );
    assert_eq!(
        is_gap_free_symbol("\"hello\" world"),
        Ok((" world", "\"hello\"".to_string()))
    );
    assert!(is_gap_free_symbol("\n").is_err());
}

#[test]
fn test_output_of_terminal_string() {
    assert!(is_terminal_string("'hello'").is_ok());
    assert!(is_terminal_string("\"world\"").is_ok());
}

#[test]
fn test_output_of_is_comment_symbol() {
    let (leftover, result) = many0(is_comment_symbol)("Some comment*)").unwrap();
    assert_eq!(leftover, "*)");
    assert_eq!(
        result,
        vec!["Some".to_string(), " ".to_string(), "comment".to_string()]
    );
}

#[test]
fn test_output_of_is_bracketed_textual_comment() {
    let (leftover, result) = is_bracketed_textual_comment(r##"(*Some comment*)"##).unwrap();
    assert_eq!(leftover, "");
    assert_eq!(result, "(*Some comment*)");
}

#[test]
fn test_output_of_printable_syntax() {
    let (_, gap_free_syntax) = is_printable_syntax(
        r##"
        (*test comment*)
        (*second comment*)
        letter = 'a' | 'b';"##,
    )
    .unwrap();
    // TODO: Fix this so that whitespaces inside comments are preserved
    assert_eq!(
        gap_free_syntax,
        "(*testcomment*)(*secondcomment*)letter='a'|'b';"
    );
}

#[test]
fn test_output_of_commentless_syntax_with_comment_only() {
    let (_, gap_free_syntax) = is_printable_syntax(
        r##"
        (*test comment*)
        (*second comment*)
        letter = 'a' | 'b';"##,
    )
    .unwrap();
    // TODO: Fix this so that it also returns the comments
    let (_, commentless_syntax) = is_commentless_syntax(&gap_free_syntax).unwrap();
    assert_eq!(commentless_syntax, "letter='a'|'b';")
}

#[test]
fn test_output_of_syntac_primary_with_empty_sequence() {
    let (_, gap_free_syntax) = is_syntactic_primary(r##""##).unwrap();
    assert_eq!(gap_free_syntax, SyntacticPrimary::EmptySequence)
}

#[test]
fn test_output_of_commentless_syntax() {
    let input = r##"
    (*
    The syntax of Extended BNF can be defined using
    itself.
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
    (* see 7.2 *) decimal digit
    = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7'
    | '8' | '9';"##;
    let (leftover_1, printable_syntax) = is_printable_syntax(input).unwrap();
    assert_eq!(leftover_1, "");
    let (leftover_2, commentless_syntax) = is_commentless_syntax(&printable_syntax).unwrap();
    assert_eq!(leftover_2, "");
    assert_eq!(commentless_syntax, "letter='a'|'b'|'c'|'d'|'e'|'f'|'g'|'h'|'i'|'j'|'k'|'l'|'m'|'n'|'o'|'p'|'q'|'r'|'s'|'t'|'u'|'v'|'w'|'x'|'y'|'z'|'A'|'B'|'C'|'D'|'E'|'F'|'G'|'H'|'I'|'J'|'K'|'L'|'M'|'N'|'O'|'P'|'Q'|'R'|'S'|'T'|'U'|'V'|'W'|'X'|'Y'|'Z';decimaldigit='0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9';");
}

#[test]
fn test_parse_informal_ebnf_itself() {
    let input = r##"
    (*
        This example defines Extended BNF
        informally. Many of the syntax rules include
        a comment to explain their meaning; inside a
        comment a meta identifier is enclosed in angle
        brackets < and > to avoid confusion with
        similar English words. The non-terminal symbols
        <letter>, <decimal digit> and <character> are
        not defined. The position of <comments> is
        stated in a comment but not formally defined.
        *)
        syntax = syntax rule, {syntax rule};
        syntax rule
        = meta identifier, '=', definitions list, ';'
        (* A <syntax rule> defines the sequences of
        symbols represented by a <meta identifier> *);
        definitions list
        = single definition, {'|', single definition}
        (* | separates alternative
        <single definitions> *);
        single definition = term, {',', term}
        (* , separates successive <terms> *);
        term = factor, ['-', exception]
        (* A <term> represents any sequence of symbols
        that is defined by the <factor> but
        not defined by the <exception> *);
        exception = factor
        (* A <factor> may be used as an <exception>
        if it could be replaced by a <factor>
        containing no <meta identifiers> *);
        factor = [integer, '*'], primary
        (* The <integer> specifies the number of
        repetitions of the <primary> *);
        primary
        = optional sequence | repeated sequence
        | special sequence | grouped sequence
        | meta identifier | terminal string | empty;
        empty = ;
        optional sequence = '[', definitions list, ']'
        (* The brackets [ and ] enclose symbols
        which are optional *);
        repeated sequence = '{', definitions list, '}'
        (* The brackets { and } enclose symbols
        which may be repeated any number of times *);
        grouped sequence = '(', definitions list, ')'
        (* The brackets ( and ) allow any
        <definitions list> to be a <primary> *);
        terminal string
        = "'", character - "'", {character - "'"}, "'"
        | '"', character - '"', {character - '"'}, '"'
        (* A <terminal string> represents the
        <characters> between the quote symbols
        '_' or "_" *);
        meta identifier = letter, {letter | decimal digit}
        (* A <meta identifier> is the name of a
        syntactic element of the language being
        defined *);
        integer = decimal digit, {decimal digit};
        special sequence = '?', {character - '?'}, '?'
        (* The meaning of a <special sequence> is not
        defined in the standard metalanguage. *);
        comment = '(*', {comment symbol}, '*)'
        (* A comment is allowed anywhere outside a
        <terminal string>, <meta identifier>,
        <integer> or <special sequence> *);
        comment symbol
        = comment | terminal string | special sequence
        | character;
    "##;
    let (leftover_1, printable_syntax) = is_printable_syntax(input).unwrap();
    assert_eq!(leftover_1, "");
    let (leftover_2, commentless_syntax) = is_commentless_syntax(&printable_syntax).unwrap();
    assert_eq!(leftover_2, "");
    assert_eq!(commentless_syntax, "syntax=syntaxrule,{syntaxrule};syntaxrule=metaidentifier,'=',definitionslist,';';definitionslist=singledefinition,{'|',singledefinition};singledefinition=term,{',',term};term=factor,['-',exception];exception=factor;factor=[integer,'*'],primary;primary=optionalsequence|repeatedsequence|specialsequence|groupedsequence|metaidentifier|terminalstring|empty;empty=;optionalsequence='[',definitionslist,']';repeatedsequence='{',definitionslist,'}';groupedsequence='(',definitionslist,')';terminalstring=\"'\",character-\"'\",{character-\"'\"},\"'\"|'\"',character-'\"',{character-'\"'},'\"';metaidentifier=letter,{letter|decimaldigit};integer=decimaldigit,{decimaldigit};specialsequence='?',{character-'?'},'?';comment='(*',{commentsymbol},'*)';commentsymbol=comment|terminalstring|specialsequence|character;");
    let (leftover_3, ebnf_syntax) = is_ebnf_syntax(&commentless_syntax).unwrap();
    assert_eq!(leftover_3, "");
    let expected_ebnf_syntax = EbnfSyntax {
        rules: vec![
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "syntaxrule".to_string(),
                                    ),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::Repeat(DefinitionList {
                                        definitions: vec![SingleDefinition {
                                            terms: vec![SyntacticTerm {
                                                factor: SyntacticFactor {
                                                    count: 1,
                                                    primary: SyntacticPrimary::MetaIdentifier(
                                                        "syntaxrule".to_string(),
                                                    ),
                                                },
                                                except: None,
                                            }],
                                        }],
                                    }),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "metaidentifier".to_string(),
                                    ),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("'='".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "definitionslist".to_string(),
                                    ),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("';'".to_string()),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "singledefinition".to_string(),
                                    ),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::Repeat(DefinitionList {
                                        definitions: vec![SingleDefinition {
                                            terms: vec![
                                                SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::TerminalString(
                                                            "'|'".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                },
                                                SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::MetaIdentifier(
                                                            "singledefinition".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                },
                                            ],
                                        }],
                                    }),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier("term".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::Repeat(DefinitionList {
                                        definitions: vec![SingleDefinition {
                                            terms: vec![
                                                SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::TerminalString(
                                                            "','".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                },
                                                SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::MetaIdentifier(
                                                            "term".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                },
                                            ],
                                        }],
                                    }),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier("factor".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::Optional(DefinitionList {
                                        definitions: vec![SingleDefinition {
                                            terms: vec![
                                                SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::TerminalString(
                                                            "'-'".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                },
                                                SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::MetaIdentifier(
                                                            "exception".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                },
                                            ],
                                        }],
                                    }),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![SyntacticTerm {
                            factor: SyntacticFactor {
                                count: 1,
                                primary: SyntacticPrimary::MetaIdentifier("factor".to_string()),
                            },
                            except: None,
                        }],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::Optional(DefinitionList {
                                        definitions: vec![SingleDefinition {
                                            terms: vec![
                                                SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::MetaIdentifier(
                                                            "integer".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                },
                                                SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::TerminalString(
                                                            "'*'".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                },
                                            ],
                                        }],
                                    }),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "primary".to_string(),
                                    ),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "optionalsequence".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "repeatedsequence".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "specialsequence".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "groupedsequence".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "metaidentifier".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "terminalstring".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier("empty".to_string()),
                                },
                                except: None,
                            }],
                        },
                    ],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![SyntacticTerm {
                            factor: SyntacticFactor {
                                count: 1,
                                primary: SyntacticPrimary::EmptySequence,
                            },
                            except: None,
                        }],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("'['".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "definitionslist".to_string(),
                                    ),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("']'".to_string()),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("'{'".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "definitionslist".to_string(),
                                    ),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("'}'".to_string()),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("'('".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "definitionslist".to_string(),
                                    ),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("')'".to_string()),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![
                        SingleDefinition {
                            terms: vec![
                                SyntacticTerm {
                                    factor: SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::TerminalString(
                                            "\"'\"".to_string(),
                                        ),
                                    },
                                    except: None,
                                },
                                SyntacticTerm {
                                    factor: SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::MetaIdentifier(
                                            "character".to_string(),
                                        ),
                                    },
                                    except: Some(SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::TerminalString(
                                            "\"'\"".to_string(),
                                        ),
                                    }),
                                },
                                SyntacticTerm {
                                    factor: SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::Repeat(DefinitionList {
                                            definitions: vec![SingleDefinition {
                                                terms: vec![SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::MetaIdentifier(
                                                            "character".to_string(),
                                                        ),
                                                    },
                                                    except: Some(SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::TerminalString(
                                                            "\"'\"".to_string(),
                                                        ),
                                                    }),
                                                }],
                                            }],
                                        }),
                                    },
                                    except: None,
                                },
                                SyntacticTerm {
                                    factor: SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::TerminalString(
                                            "\"'\"".to_string(),
                                        ),
                                    },
                                    except: None,
                                },
                            ],
                        },
                        SingleDefinition {
                            terms: vec![
                                SyntacticTerm {
                                    factor: SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::TerminalString(
                                            "'\"'".to_string(),
                                        ),
                                    },
                                    except: None,
                                },
                                SyntacticTerm {
                                    factor: SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::MetaIdentifier(
                                            "character".to_string(),
                                        ),
                                    },
                                    except: Some(SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::TerminalString(
                                            "'\"'".to_string(),
                                        ),
                                    }),
                                },
                                SyntacticTerm {
                                    factor: SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::Repeat(DefinitionList {
                                            definitions: vec![SingleDefinition {
                                                terms: vec![SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::MetaIdentifier(
                                                            "character".to_string(),
                                                        ),
                                                    },
                                                    except: Some(SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::TerminalString(
                                                            "'\"'".to_string(),
                                                        ),
                                                    }),
                                                }],
                                            }],
                                        }),
                                    },
                                    except: None,
                                },
                                SyntacticTerm {
                                    factor: SyntacticFactor {
                                        count: 1,
                                        primary: SyntacticPrimary::TerminalString(
                                            "'\"'".to_string(),
                                        ),
                                    },
                                    except: None,
                                },
                            ],
                        },
                    ],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier("letter".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::Repeat(DefinitionList {
                                        definitions: vec![
                                            SingleDefinition {
                                                terms: vec![SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::MetaIdentifier(
                                                            "letter".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                }],
                                            },
                                            SingleDefinition {
                                                terms: vec![SyntacticTerm {
                                                    factor: SyntacticFactor {
                                                        count: 1,
                                                        primary: SyntacticPrimary::MetaIdentifier(
                                                            "decimaldigit".to_string(),
                                                        ),
                                                    },
                                                    except: None,
                                                }],
                                            },
                                        ],
                                    }),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "decimaldigit".to_string(),
                                    ),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::Repeat(DefinitionList {
                                        definitions: vec![SingleDefinition {
                                            terms: vec![SyntacticTerm {
                                                factor: SyntacticFactor {
                                                    count: 1,
                                                    primary: SyntacticPrimary::MetaIdentifier(
                                                        "decimaldigit".to_string(),
                                                    ),
                                                },
                                                except: None,
                                            }],
                                        }],
                                    }),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("'?'".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::Repeat(DefinitionList {
                                        definitions: vec![SingleDefinition {
                                            terms: vec![SyntacticTerm {
                                                factor: SyntacticFactor {
                                                    count: 1,
                                                    primary: SyntacticPrimary::MetaIdentifier(
                                                        "character".to_string(),
                                                    ),
                                                },
                                                except: Some(SyntacticFactor {
                                                    count: 1,
                                                    primary: SyntacticPrimary::TerminalString(
                                                        "'?'".to_string(),
                                                    ),
                                                }),
                                            }],
                                        }],
                                    }),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("'?'".to_string()),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![SingleDefinition {
                        terms: vec![
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("'(*'".to_string()),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::Repeat(DefinitionList {
                                        definitions: vec![SingleDefinition {
                                            terms: vec![SyntacticTerm {
                                                factor: SyntacticFactor {
                                                    count: 1,
                                                    primary: SyntacticPrimary::MetaIdentifier(
                                                        "commentsymbol".to_string(),
                                                    ),
                                                },
                                                except: None,
                                            }],
                                        }],
                                    }),
                                },
                                except: None,
                            },
                            SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::TerminalString("'*)'".to_string()),
                                },
                                except: None,
                            },
                        ],
                    }],
                },
            },
            SyntaxRule {
                name: "=".to_string(),
                definition: DefinitionList {
                    definitions: vec![
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "comment".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "terminalstring".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "specialsequence".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                        SingleDefinition {
                            terms: vec![SyntacticTerm {
                                factor: SyntacticFactor {
                                    count: 1,
                                    primary: SyntacticPrimary::MetaIdentifier(
                                        "character".to_string(),
                                    ),
                                },
                                except: None,
                            }],
                        },
                    ],
                },
            },
        ],
    };
    assert_eq!(ebnf_syntax, expected_ebnf_syntax);
}
