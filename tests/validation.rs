#[test]
pub fn use_syntax_to_validate() {
    let syntax = iso_14977::parse(
        r#"
    digit excluding zero = "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" ;
    digit                = "0" | digit excluding zero ;
    natural number       = digit excluding zero, { digit } ;"#,
    )
    .unwrap();
    println!("{:#?}", syntax);
}
