use nom::{
    branch::alt,
    bytes::complete::{tag, take_while_m_n},
    character::complete::one_of,
    combinator::{consumed, map_res, not, recognize, verify},
    multi::many0,
    sequence::delimited,
    sequence::tuple,
    IResult as NomResult, Parser,
};

macro_rules! ebnf_characters_functions {
    ($func_name:ident, $out:ty, $pattern:expr) => {
        fn $func_name(input: &str) -> NomResult<&str, $out> {
            ($pattern)(input)
        }
    };

    ($func_name:ident, $out:ty, $($pattern:expr),+) => {
        fn $func_name(input: &str) -> NomResult<&str, &str> {
            alt((
                $($pattern),+
            ))(input)
        }
    };
}

// The first part of the lexical syntax defines the characters in the 7-bit
// character set (ISO/IEC 646:1991) that represent each terminal-character
// and gap-separator in Extended BNF.

ebnf_characters_functions!(
    is_letter,
    &str,
    recognize(one_of(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ))
);
ebnf_characters_functions!(is_decimal_digit, &str, recognize(one_of("0123456789")));
ebnf_characters_functions!(is_concatenate_symbol, &str, tag(","));
ebnf_characters_functions!(is_defining_symbol, &str, tag("="));
ebnf_characters_functions!(is_definition_separator, &str, recognize(one_of("|/!")));
ebnf_characters_functions!(is_end_comment_symbol, &str, tag("*)"));
ebnf_characters_functions!(is_end_group_symbol, &str, tag(")"));
ebnf_characters_functions!(is_end_option_symbol, &str, tag("]"), tag("/)"));
ebnf_characters_functions!(is_end_repeat_symbol, &str, tag("}"), tag(":)"));
ebnf_characters_functions!(is_except_symbol, &str, tag("-"));
ebnf_characters_functions!(is_first_quote_symbol, &str, tag("'"));
ebnf_characters_functions!(is_repetition_symbol, &str, tag("*"));
ebnf_characters_functions!(is_second_quote_symbol, &str, tag("\""));
ebnf_characters_functions!(is_special_sequence_symbol, &str, tag("?"));
ebnf_characters_functions!(is_start_comment_symbol, &str, tag("(*"));
ebnf_characters_functions!(is_start_group_symbol, &str, tag("("));
ebnf_characters_functions!(is_start_option_symbol, &str, tag("["), tag("(/"));
ebnf_characters_functions!(is_start_repeat_symbol, &str, tag("{"), tag("(:"));
ebnf_characters_functions!(is_terminator_symbol, &str, recognize(one_of(";.")));
ebnf_characters_functions!(
    is_other_character,
    &str,
    recognize(one_of(" :+_%@&#$<>\\^`~"))
);
ebnf_characters_functions!(is_space_character, &str, tag(" "));
ebnf_characters_functions!(is_horizontal_tabulation_character, &str, tag("\t"));
ebnf_characters_functions!(
    is_new_line,
    &str,
    delimited(many0(tag("\r")), tag("\n"), many0(tag("\r")))
);
ebnf_characters_functions!(
    is_vertical_tabulation_character,
    &str,
    tag("\\v"),
    tag("\u{000B}")
);
ebnf_characters_functions!(is_form_feed, &str, tag("\\f"));

// Second part fo the syntax defines the removal of unnecessary non-printing characters from a syntax

ebnf_characters_functions!(
    is_terminal_character,
    &str,
    alt((
        is_letter,
        is_decimal_digit,
        is_concatenate_symbol,
        is_defining_symbol,
        is_definition_separator,
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
ebnf_characters_functions!(
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
ebnf_characters_functions!(
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
ebnf_characters_functions!(
    is_first_terminal_character,
    &str,
    verify(is_terminal_character, |o| {
        is_first_quote_symbol(o).is_err()
    })
);
ebnf_characters_functions!(
    is_second_terminal_character,
    &str,
    verify(is_terminal_character, |o| {
        is_second_quote_symbol(o).is_err()
    })
);
ebnf_characters_functions!(
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
ebnf_characters_functions!(
    is_syntax,
    String,
    alt((tuple((
        many0(is_gap_separator),
        is_gap_free_symbol,
        many0(is_gap_separator),
        many0(tuple((is_gap_free_symbol, many0(is_gap_separator)))),
    ))
    .map(|(a, b, c, d)| {
        format!(
            "{}{}{}{}",
            a.join(""),
            b,
            c.join(""),
            d.iter()
                .map(|(e, f)| format!("{}{}", e, f.join("")))
                .collect::<String>()
        )
    }),))
);

// The third part of the syntax defines the removal of bracketed-textual-comments
// from gap-free-symbols that form a syntax.

ebnf_characters_functions!(
    is_commentless_symbol,
    String,
    alt((
        verify(is_terminal_character, |o| {
            not(alt((
                is_letter,
                is_decimal_digit,
                is_first_quote_symbol,
                is_second_quote_symbol,
                is_start_comment_symbol,
                is_end_comment_symbol,
                is_special_sequence_symbol,
                is_other_character,
            )))(o)
            .is_err()
        })
        .map(|o| o.to_string()),
        is_meta_identifier,
        is_integer,
        is_terminal_string,
        is_special_sequence,
    ))
);

ebnf_characters_functions!(
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
ebnf_characters_functions!(
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
ebnf_characters_functions!(
    is_meta_identifier_character,
    &str,
    alt((is_letter, is_decimal_digit))
);
ebnf_characters_functions!(
    is_special_sequence,
    String,
    alt((tuple((
        is_special_sequence_symbol,
        many0(is_special_sequence_character),
        is_special_sequence_symbol,
    ))
    .map(|(a, b, c)| format!("{}{}{}", a, b.join(""), c)),))
);
ebnf_characters_functions!(
    is_special_sequence_character,
    &str,
    verify(is_terminal_character, |o| {
        is_special_sequence_symbol(o).is_err()
    })
);

mod tests {
    #[allow(unused_imports)]
    use crate::parser::*;

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
}
