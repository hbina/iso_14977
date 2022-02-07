use nom::multi::many_m_n;
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
    .map(|(a, b, c, d)| {
        format!(
            "{}{}",
            b,
            d.iter().map(|(e, f)| format!("{}", e)).collect::<String>()
        )
    }),))
);

// The final part of the syntax defines the abstract syntax of Extended BNF, i.e. the
// structure in terms of the commentless symbols.

ebnf_rules!(
    is_ebnf_syntax,
    String,
    alt((
        tuple((is_syntax_rule, many0(is_syntax_rule))).map(|(a, b)| format!("{}{}", a, b.join(""))),
    ))
);
ebnf_rules!(
    is_syntax_rule,
    String,
    alt((tuple((
        is_meta_identifier,
        is_defining_symbol,
        is_definition_list,
        is_terminator_symbol
    ))
    .map(|(a, b, c, d)| format!("{}{}{}{}", a, b, c, d)),))
);
ebnf_rules!(
    is_definition_list,
    String,
    alt((tuple((
        is_single_definition,
        many0(tuple((
            is_definition_separator_symbol,
            is_single_definition
        )))
    ))
    .map(|(a, b)| format!(
        "{}{}",
        a,
        b.iter()
            .map(|(c, d)| format!("{}{}", c, d))
            .collect::<String>()
    )),))
);
ebnf_rules!(
    is_single_definition,
    String,
    alt((tuple((
        is_syntactic_term,
        many0(tuple((is_concatenate_symbol, is_syntactic_term)))
    ))
    .map(|(a, b)| format!(
        "{}{}",
        a,
        b.iter()
            .map(|(c, d)| format!("{}{}", c, d))
            .collect::<String>()
    )),))
);
ebnf_rules!(
    is_syntactic_term,
    String,
    alt((tuple((
        is_syntactic_factor,
        many_m_n(0, 1, tuple((is_except_symbol, is_syntax_exception)))
    ))
    .map(|(a, b)| format!(
        "{}{}",
        a,
        b.iter()
            .map(|(c, d)| format!("{}{}", c, d))
            .collect::<String>()
    )),))
);
// FIXME: Currentely unused
ebnf_rules!(is_syntax_exception, String, is_syntactic_factor);
ebnf_rules!(
    is_syntactic_factor,
    String,
    alt((tuple((
        many_m_n(0, 1, tuple((is_integer, is_repetition_symbol))),
        is_syntactic_primary
    ))
    .map(|(a, b)| format!(
        "{}{}",
        a.iter()
            .map(|(c, d)| format!("{}{}", c, d))
            .collect::<String>(),
        b
    )),))
);
ebnf_rules!(
    is_syntactic_primary,
    String,
    // NOTE: Use many_m_n to encode empty sequence
    alt((many_m_n(
        0,
        1,
        alt((
            is_optional_sequence,
            is_repeated_sequence,
            is_grouped_sequence,
            is_meta_identifier,
            is_terminal_string,
            is_special_sequence,
            // is_empty_sequence
        ))
    )
    .map(|a| a.join("")),))
);
ebnf_rules!(
    is_optional_sequence,
    String,
    alt((tuple((
        is_start_option_symbol,
        is_definition_list,
        is_end_option_symbol
    ))
    .map(|(a, b, c)| format!("{}{}{}", a, b, c)),))
);
ebnf_rules!(
    is_repeated_sequence,
    String,
    alt((tuple((
        is_start_repeat_symbol,
        is_definition_list,
        is_end_repeat_symbol
    ))
    .map(|(a, b, c)| format!("{}{}{}", a, b, c)),))
);
ebnf_rules!(
    is_grouped_sequence,
    String,
    alt((tuple((
        is_start_group_symbol,
        is_definition_list,
        is_end_group_symbol
    ))
    .map(|(a, b, c)| format!("{}{}{}", a, b, c)),))
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
        assert_eq!(
            ebnf_syntax,
            "syntax=syntaxrule,{syntaxrule};syntaxrule=metaidentifier,'=',definitionslist,';';definitionslist=singledefinition,{'|',singledefinition};singledefinition=term,{',',term};term=factor,['-',exception];exception=factor;factor=[integer,'*'],primary;primary=optionalsequence|repeatedsequence|specialsequence|groupedsequence|metaidentifier|terminalstring|empty;empty=;optionalsequence='[',definitionslist,']';repeatedsequence='{',definitionslist,'}';groupedsequence='(',definitionslist,')';terminalstring=\"'\",character-\"'\",{character-\"'\"},\"'\"|'\"',character-'\"',{character-'\"'},'\"';metaidentifier=letter,{letter|decimaldigit};integer=decimaldigit,{decimaldigit};specialsequence='?',{character-'?'},'?';comment='(*',{commentsymbol},'*)';commentsymbol=comment|terminalstring|specialsequence|character;"
        );
    }
}
