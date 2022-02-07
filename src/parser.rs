#[allow(dead_code)]
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

macro_rules! ebnf_rules {
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

ebnf_rules!(
    is_letter,
    &str,
    recognize(one_of(
        "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    ))
);
ebnf_rules!(is_decimal_digit, &str, recognize(one_of("0123456789")));
ebnf_rules!(is_concatenate_symbol, &str, tag(","));
ebnf_rules!(is_defining_symbol, &str, tag("="));
ebnf_rules!(is_definition_separator, &str, recognize(one_of("|/!")));
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

ebnf_rules!(
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
    .map(|(a, b, c, d)| {
        format!(
            "{}{}",
            b,
            d.iter().map(|(e, f)| format!("{}", e)).collect::<String>()
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
    .map(|(a, b, c, d)| format!(
        "{}{}",
        b,
        d.iter().map(|(e, f)| format!("{}", e)).collect::<String>()
    )),))
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

    #[test]
    fn test_output_of_is_comment_symbol() {
        assert_eq!(
            many0(is_comment_symbol)("Some comment*)"),
            Ok((
                "*)",
                vec!["Some".to_string(), " ".to_string(), "comment".to_string()]
            ))
        );
    }

    #[test]
    fn test_output_of_is_bracketed_textual_comment() {
        let result = is_bracketed_textual_comment(r##"(*Some comment*)"##).unwrap();
        println!("result:\n{:#?}", result);
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
        let (_, commentless_syntax) = is_commentless_syntax(&gap_free_syntax).unwrap();
        assert_eq!(commentless_syntax, "letter='a'|'b';")
    }

    #[test]
    fn test_output_of_commentless_syntax() {
        let result = is_printable_syntax(
            r##"
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
| '^' | '`' | '~';
(* see 7.6 *) space_character = ' ';
                "##,
        )
        .unwrap();
        println!("result:\n{:#?}", result);
    }
}
