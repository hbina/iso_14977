use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::one_of,
    combinator::{opt, recognize, verify},
    multi::many0,
    sequence::delimited,
    sequence::tuple,
    IResult as NomResult, Parser,
};

use crate::{EBNFError, EBNFResult};

macro_rules! ebnf_rules {
    ($func_name:ident, $out:ty, $pattern:expr) => {
        pub fn $func_name(input: &str) -> NomResult<&str, $out> {
            ($pattern)(input)
        }
    };

    ($func_name:ident, $out:ty, $($pattern:expr),+) => {
        pub fn $func_name(input: &str) -> NomResult<&str, &str> {
            alt((
                $($pattern),+
            ))(input)
        }
    };
}

// The first part of the lexical syntax defines the characters in the 7-bit
// character set (ISO/IEC 646:1991) that represent each terminal-character
// and gap-separator in Extended BNF.

// letter
// = 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h'
// | 'i' | 'j' | 'k' | 'l' | 'm' | 'n' | 'o' | 'p'
// | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x'
// | 'y' | 'z'
// | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H'
// | 'I' | 'J' | 'K' | 'L' | 'M' | 'N' | 'O' | 'P'
// | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X'
// | 'Y' | 'Z';
ebnf_rules!(
    is_letter,
    &str,
    recognize(one_of(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ))
);
// decimal digit
// = '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7'
// | '8' | '9';
ebnf_rules!(is_decimal_digit, &str, recognize(one_of("0123456789")));
ebnf_rules!(is_concatenate_symbol, &str, tag(","));
ebnf_rules!(is_defining_symbol, &str, tag("="));
ebnf_rules!(
    is_definition_separator_symbol,
    &str,
    recognize(one_of("|/!"))
);
ebnf_rules!(is_end_comment_symbol, &str, tag("*)"));
ebnf_rules!(is_end_group_symbol, &str, tag(")"));
ebnf_rules!(is_end_option_symbol, &str, tag("]"), tag("/)"));
ebnf_rules!(is_end_repeat_symbol, &str, tag("}"), tag(":)"));
ebnf_rules!(is_except_symbol, &str, tag("-"));
ebnf_rules!(is_first_quote_symbol, &str, tag("'"));
ebnf_rules!(is_repetition_symbol, &str, tag("*"));
ebnf_rules!(is_second_quote_symbol, &str, tag("\""));
ebnf_rules!(is_special_sequence_symbol, &str, tag("?"));
ebnf_rules!(is_start_comment_symbol, &str, tag("(*"));
ebnf_rules!(is_start_group_symbol, &str, tag("("));
ebnf_rules!(is_start_option_symbol, &str, tag("["), tag("(/"));
ebnf_rules!(is_start_repeat_symbol, &str, tag("{"), tag("(:"));
ebnf_rules!(is_terminator_symbol, &str, recognize(one_of(";.")));
ebnf_rules!(
    is_other_character,
    &str,
    recognize(one_of(" :+_%@&#$<>\\^`~"))
);
ebnf_rules!(is_space_character, &str, tag(" "));
ebnf_rules!(is_horizontal_tabulation_character, &str, tag("\t"));
ebnf_rules!(
    is_new_line,
    &str,
    delimited(many0(tag("\r")), tag("\n"), many0(tag("\r")))
);
ebnf_rules!(
    is_vertical_tabulation_character,
    &str,
    tag("\\v"),
    tag("\u{000B}")
);
ebnf_rules!(is_form_feed, &str, tag("\\f"));

// Second part fo the syntax defines the removal of unnecessary non-printing characters from a syntax

// terminal character
// = letter
// | decimal digit
// | concatenate symbol
// | defining symbol
// | definition separator symbol
// | end comment symbol
// | end group symbol
// | end option symbol
// | end repeat symbol
// | except symbol
// | first quote symbol
// | repetition symbol
// | second quote symbol
// | special sequence symbol
// | start comment symbol
// | start group symbol
// | start option symbol
// | start repeat symbol
// | terminator symbol
// | other character;
ebnf_rules!(
    is_terminal_character,
    &str,
    alt((
        is_letter,
        is_decimal_digit,
        is_concatenate_symbol,
        is_defining_symbol,
        is_definition_separator_symbol,
        is_end_comment_symbol,
        is_end_group_symbol,
        is_end_option_symbol,
        is_end_repeat_symbol,
        is_except_symbol,
        is_first_quote_symbol,
        is_repetition_symbol,
        is_second_quote_symbol,
        is_special_sequence_symbol,
        is_start_comment_symbol,
        is_start_group_symbol,
        is_start_option_symbol,
        is_start_repeat_symbol,
        is_terminator_symbol,
        is_other_character
    ))
);
ebnf_rules!(
    is_gap_free_symbol,
    String,
    alt((
        verify(is_terminal_character, |o: &str| {
            alt((is_first_quote_symbol, is_second_quote_symbol))(o).is_err()
        })
        .map(|d| format!("{}", d)),
        is_terminal_string,
    ))
);
ebnf_rules!(
    is_terminal_string,
    String,
    alt((alt((
        tuple((
            is_first_quote_symbol,
            is_first_terminal_character,
            many0(is_first_terminal_character),
            is_first_quote_symbol,
        )),
        tuple((
            is_second_quote_symbol,
            is_second_terminal_character,
            many0(is_second_terminal_character),
            is_second_quote_symbol,
        )),
    ))
    .map(|(a, b, c, d)| format!("{}{}{}{}", a, b, c.join(""), d)),))
);
ebnf_rules!(
    is_first_terminal_character,
    &str,
    verify(is_terminal_character, |o| {
        is_first_quote_symbol(o).is_err()
    })
);
ebnf_rules!(
    is_second_terminal_character,
    &str,
    verify(is_terminal_character, |o| {
        is_second_quote_symbol(o).is_err()
    })
);
ebnf_rules!(
    is_gap_separator,
    &str,
    alt((
        is_space_character,
        is_horizontal_tabulation_character,
        is_new_line,
        is_vertical_tabulation_character,
        is_form_feed
    ))
);
ebnf_rules!(
    is_printable_syntax,
    String,
    alt((tuple((
        many0(is_gap_separator),
        is_gap_free_symbol,
        many0(is_gap_separator),
        many0(tuple((is_gap_free_symbol, many0(is_gap_separator)))),
    ))
    .map(|(_, b, _, d)| {
        format!(
            "{}{}",
            b,
            d.iter().map(|(e, _)| format!("{}", e)).collect::<String>()
        )
    }),))
);

// The third part of the syntax defines the removal of bracketed-textual-comments
// from gap-free-symbols that form a syntax.

ebnf_rules!(
    is_commentless_symbol,
    String,
    alt((
        verify(is_terminal_character, |o| {
            alt((
                is_letter,
                is_decimal_digit,
                is_first_quote_symbol,
                is_second_quote_symbol,
                is_start_comment_symbol,
                is_end_comment_symbol,
                is_special_sequence_symbol,
                is_other_character,
            ))(o)
            .is_err()
        })
        .map(|o| o.to_string()),
        is_meta_identifier,
        is_integer,
        is_terminal_string,
        is_special_sequence,
    ))
);
ebnf_rules!(
    is_integer,
    String,
    alt((
        tuple((is_decimal_digit, many0(is_decimal_digit))).map(|(a, b)| format!(
            "{}{}",
            a,
            b.join("")
        )),
    ))
);
ebnf_rules!(
    is_meta_identifier,
    String,
    alt((
        tuple((is_letter, many0(is_meta_identifier_character))).map(|(a, b)| format!(
            "{}{}",
            a,
            b.join("")
        )),
    ))
);
// FIXME: Do we want to support underscores here?
ebnf_rules!(
    is_meta_identifier_character,
    &str,
    alt((is_letter, is_decimal_digit))
);
ebnf_rules!(
    is_special_sequence,
    String,
    alt((tuple((
        is_special_sequence_symbol,
        many0(is_special_sequence_character),
        is_special_sequence_symbol,
    ))
    .map(|(a, b, c)| format!("{}{}{}", a, b.join(""), c)),))
);
ebnf_rules!(
    is_special_sequence_character,
    &str,
    verify(is_terminal_character, |o| {
        is_special_sequence_symbol(o).is_err()
    })
);
ebnf_rules!(
    is_comment_symbol,
    String,
    alt((
        is_bracketed_textual_comment,
        is_other_character.map(|d| { format!("{}", d) }),
        is_commentless_symbol.map(|d| { format!("{}", d) }),
    ))
);
ebnf_rules!(
    is_bracketed_textual_comment,
    String,
    alt((tuple((
        is_start_comment_symbol,
        many0(is_comment_symbol),
        is_end_comment_symbol
    ))
    .map(|(a, b, c)| format!("{}{}{}", a, b.join(""), c)),))
);
ebnf_rules!(
    is_commentless_syntax,
    String,
    alt((tuple((
        many0(is_bracketed_textual_comment),
        is_commentless_symbol,
        many0(is_bracketed_textual_comment),
        many0(tuple((
            is_commentless_symbol,
            many0(is_bracketed_textual_comment)
        )))
    ))
    .map(|(_, b, _, d)| {
        format!(
            "{}{}",
            b,
            d.iter().map(|(e, _)| format!("{}", e)).collect::<String>()
        )
    }),))
);

// The final part of the syntax defines the abstract syntax of Extended BNF, i.e. the
// structure in terms of the commentless symbols.

// (* see 4.2 *) syntax
// = syntax rule, {syntax rule};
ebnf_rules!(
    is_ebnf_syntax,
    EbnfSyntax,
    alt((
        tuple((is_syntax_rule, many0(is_syntax_rule))).map(|(a, mut b)| {
            let mut rules = vec![a];
            rules.append(&mut b);
            EbnfSyntax { rules }
        }),
    ))
);
#[derive(Debug, PartialEq, Eq)]
pub struct EbnfSyntax {
    pub rules: Vec<SyntaxRule>,
}

// (* see 4.3 *) syntax rule
// = meta identifier, defining symbol,
// definitions list, terminator symbol;
ebnf_rules!(
    is_syntax_rule,
    SyntaxRule,
    alt((tuple((
        is_meta_identifier,
        is_defining_symbol,
        is_definition_list,
        is_terminator_symbol
    ))
    .map(|(_, b, c, _)| SyntaxRule {
        name: b.to_string(),
        definition: c
    }),))
);
#[derive(Debug, PartialEq, Eq)]
pub struct SyntaxRule {
    pub name: String,
    pub definition: DefinitionList,
}

// (* see 4.4 *) definitions list
// = single definition,
// {definition separator symbol,
// single definition};
ebnf_rules!(
    is_definition_list,
    DefinitionList,
    alt((tuple((
        is_single_definition,
        many0(tuple((
            is_definition_separator_symbol,
            is_single_definition
        )))
    ))
    .map(|(a, b)| {
        let mut definitions = vec![a];
        definitions.extend(b.into_iter().map(|v| v.1));
        DefinitionList { definitions }
    }),))
);
#[derive(Debug, PartialEq, Eq)]
pub struct DefinitionList {
    pub definitions: Vec<SingleDefinition>,
}

// (* see 4.5 *) single definition
// = syntactic term,
// {concatenate symbol, syntactic term};
ebnf_rules!(
    is_single_definition,
    SingleDefinition,
    alt((tuple((
        is_syntactic_term,
        many0(tuple((is_concatenate_symbol, is_syntactic_term)))
    ))
    .map(|(a, b)| {
        let mut terms = vec![a];
        terms.extend(b.into_iter().map(|v| v.1));
        SingleDefinition { terms }
    }),))
);
#[derive(Debug, PartialEq, Eq)]
pub struct SingleDefinition {
    pub terms: Vec<SyntacticTerm>,
}

// (* see 4.6 *) syntactic term
// = syntactic factor,
// [except symbol, syntactic exception];
ebnf_rules!(
    is_syntactic_term,
    SyntacticTerm,
    alt((tuple((
        is_syntactic_factor,
        opt(tuple((is_except_symbol, is_syntax_exception)))
    ))
    .map(|(a, b)| SyntacticTerm {
        factor: a,
        except: b.map(|v| v.1)
    }),))
);
#[derive(Debug, PartialEq, Eq)]
pub struct SyntacticTerm {
    pub factor: SyntacticFactor,
    pub except: Option<SyntacticFactor>,
}

// FIXME: Currentely unused
ebnf_rules!(is_syntax_exception, SyntacticFactor, is_syntactic_factor);

// (* see 4.8 *) syntactic factor
// = [integer, repetition symbol],
// syntactic primary
ebnf_rules!(
    is_syntactic_factor,
    SyntacticFactor,
    alt((tuple((
        opt(tuple((is_integer, is_repetition_symbol))),
        is_syntactic_primary
    ))
    .map(|(a, b)| SyntacticFactor {
        count: a
            .map(|(l, _)| l.parse().expect("There's a bug with the is_integer parser"))
            .unwrap_or(1),
        primary: b
    }),))
);
#[derive(Debug, PartialEq, Eq)]
pub struct SyntacticFactor {
    pub count: usize,
    pub primary: SyntacticPrimary,
}

// (* see 4.10 *) syntactic primary
// = optional sequence
// | repeated sequence
// | grouped sequence
// | meta identifier
// | terminal string
// | special sequence
// | empty sequence;
ebnf_rules!(
    is_syntactic_primary,
    SyntacticPrimary,
    // NOTE: Use many_m_n to encode empty sequence
    alt((
        is_optional_sequence,
        is_repeated_sequence,
        is_grouped_sequence,
        is_meta_identifier.map(|v| SyntacticPrimary::MetaIdentifier(v)),
        is_terminal_string.map(|v| SyntacticPrimary::TerminalString(v[1..v.len() - 1].to_string())),
        is_special_sequence.map(|v| SyntacticPrimary::SpecialSequence(v)),
        opt(tag("")).map(|_| SyntacticPrimary::EmptySequence)
    ))
);
#[derive(Debug, PartialEq, Eq)]
pub enum SyntacticPrimary {
    Optional(DefinitionList),
    Repeat(DefinitionList),
    Group(DefinitionList),
    MetaIdentifier(String),
    TerminalString(String),
    SpecialSequence(String),
    EmptySequence,
}

ebnf_rules!(
    is_optional_sequence,
    SyntacticPrimary,
    alt((tuple((
        is_start_option_symbol,
        is_definition_list,
        is_end_option_symbol
    ))
    .map(|(_, b, _)| SyntacticPrimary::Optional(b)),))
);
ebnf_rules!(
    is_repeated_sequence,
    SyntacticPrimary,
    alt((tuple((
        is_start_repeat_symbol,
        is_definition_list,
        is_end_repeat_symbol
    ))
    .map(|(_, b, _)| SyntacticPrimary::Repeat(b)),))
);
ebnf_rules!(
    is_grouped_sequence,
    SyntacticPrimary,
    alt((tuple((
        is_start_group_symbol,
        is_definition_list,
        is_end_group_symbol
    ))
    .map(|(_, b, _)| SyntacticPrimary::Group(b)),))
);

// Export a convenient function
#[allow(dead_code)]
pub fn parse_ebnf(input: &str, strict_mode: bool) -> EBNFResult<EbnfSyntax> {
    let (leftover_1, printable_syntax) = is_printable_syntax(input)?;
    if !leftover_1.is_empty() && strict_mode {
        return Err(EBNFError::ExtraToken(leftover_1.to_string()));
    }
    let (leftover_2, commentless_syntax) = is_commentless_syntax(&printable_syntax)?;
    if !leftover_2.is_empty() && strict_mode {
        return Err(EBNFError::ExtraToken(leftover_2.to_string()));
    }
    let (leftover_3, ebnf_syntax) = is_ebnf_syntax(&commentless_syntax)?;
    if !leftover_3.is_empty() && strict_mode {
        return Err(EBNFError::ExtraToken(leftover_3.to_string()));
    }
    Ok(ebnf_syntax)
}
