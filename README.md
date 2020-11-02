### EBNF Parser Library (Hopefully ISO compliant!)

This repository intends to be the ISO 14977 compliant EBNF parser library in Rust.
Ironically, it uses pest to generate the syntax tree lol.

I originally needed this to validate a DNS Preferred Name Syntax in RFC 1035.
Unfortunately, I am unable to find any crates that can validate a string given some BNF rules.
Additionally, the BNF provided in the RFC is not even standard EBNF --- which is kinda sad and dissappointing.
Regardless, here it is, a hopefully ISO complaint implementation of it.
