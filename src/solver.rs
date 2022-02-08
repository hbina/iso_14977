use std::{collections::HashMap, hash::Hash, ops::Add};

use crate::{
    DefinitionList, EbnfError, EbnfSyntax, SingleDefinition, SyntacticFactor, SyntacticPrimary,
    SyntacticTerm,
};

trait Satisfy {
    type Original;
    fn from_original(original: Self::Original, context: &HashMap<String, DefinitionList>) -> Self;
    fn possible_solutions(&self) -> Box<dyn Iterator<Item = String>>;
}

#[derive(Debug)]
pub struct EbnfSolver {
    rules: HashMap<String, DefinitionListSolver>,
}

impl EbnfSolver {
    pub fn from_syntax(syntax: EbnfSyntax) -> Self {
        let context = syntax
            .rules
            .iter()
            .cloned()
            .map(|v| (v.name, v.definition))
            .collect::<HashMap<_, _>>();
        Self {
            rules: syntax
                .rules
                .into_iter()
                .map(|v| {
                    (
                        v.name,
                        DefinitionListSolver::from_original(v.definition, &context),
                    )
                })
                .collect(),
        }
    }

    pub fn solve(&self, input: &str) -> bool {
        todo!()
    }
}

#[derive(Debug)]
struct DefinitionListSolver {
    definitions: Vec<SingleDefinitionSolver>,
}

impl Satisfy for DefinitionListSolver {
    type Original = DefinitionList;

    fn from_original(original: Self::Original, context: &HashMap<String, DefinitionList>) -> Self {
        Self {
            definitions: original
                .definitions
                .into_iter()
                .map(|o| SingleDefinitionSolver::from_original(o, context))
                .collect(),
        }
    }

    fn possible_solutions(&self) -> Box<dyn Iterator<Item = String>> {
        todo!()
    }
}

#[derive(Debug)]
struct SingleDefinitionSolver {
    terms: Vec<SyntacticTermSolver>,
}

impl Satisfy for SingleDefinitionSolver {
    type Original = SingleDefinition;

    fn from_original(original: Self::Original, context: &HashMap<String, DefinitionList>) -> Self {
        Self {
            terms: original
                .terms
                .into_iter()
                .map(|o| SyntacticTermSolver::from_original(o, context))
                .collect(),
        }
    }

    fn possible_solutions(&self) -> Box<dyn Iterator<Item = String>> {
        todo!()
    }
}

#[derive(Debug)]
struct SyntacticTermSolver {
    factor: SyntacticFactorSolver,
}

impl Satisfy for SyntacticTermSolver {
    type Original = SyntacticTerm;

    fn from_original(original: Self::Original, context: &HashMap<String, DefinitionList>) -> Self {
        Self {
            factor: SyntacticFactorSolver::from_original(original.factor, context),
        }
    }

    fn possible_solutions(&self) -> Box<dyn Iterator<Item = String>> {
        todo!()
    }
}

#[derive(Debug)]
struct SyntacticFactorSolver {
    count: usize,
    primary: SyntacticPrimarySolver,
}

impl Satisfy for SyntacticFactorSolver {
    type Original = SyntacticFactor;

    fn from_original(original: Self::Original, context: &HashMap<String, DefinitionList>) -> Self {
        Self {
            count: original.count,
            primary: SyntacticPrimarySolver::from_original(original.primary, context),
        }
    }

    fn possible_solutions(&self) -> Box<dyn Iterator<Item = String>> {
        todo!()
    }
}

#[derive(Debug)]
enum SyntacticPrimarySolver {
    Optional(DefinitionListSolver),
    Repeat(DefinitionListSolver),
    Group(DefinitionListSolver),
    MetaIdentifier(DefinitionListSolver),
    TerminalString(String),
    SpecialSequence(String),
    EmptySequence,
}

impl Satisfy for SyntacticPrimarySolver {
    type Original = SyntacticPrimary;

    fn from_original(original: Self::Original, context: &HashMap<String, DefinitionList>) -> Self {
        match original {
            SyntacticPrimary::Optional(v) => {
                SyntacticPrimarySolver::Optional(DefinitionListSolver::from_original(v, context))
            }
            SyntacticPrimary::Repeat(v) => {
                SyntacticPrimarySolver::Optional(DefinitionListSolver::from_original(v, context))
            }
            SyntacticPrimary::Group(v) => {
                SyntacticPrimarySolver::Optional(DefinitionListSolver::from_original(v, context))
            }
            SyntacticPrimary::MetaIdentifier(v) => SyntacticPrimarySolver::MetaIdentifier(
                context
                    .get(&v)
                    .map(|d| DefinitionListSolver::from_original(d.clone(), context))
                    .expect(&format!("Cannot find {} in the rules", v)),
            ),
            SyntacticPrimary::TerminalString(v) => {
                SyntacticPrimarySolver::TerminalString(v.clone())
            }
            SyntacticPrimary::SpecialSequence(v) => SyntacticPrimarySolver::SpecialSequence(v),
            SyntacticPrimary::EmptySequence => SyntacticPrimarySolver::EmptySequence,
        }
    }

    fn possible_solutions(&self) -> Box<dyn Iterator<Item = String>> {
        match self {
            SyntacticPrimarySolver::Optional(v) => {
                Box::new(std::iter::once(String::new()).chain(v.possible_solutions()))
            }
            SyntacticPrimarySolver::Repeat(v) => Box::new(
                v.possible_solutions()
                    .collect::<Vec<_>>()
                    .into_iter()
                    .cycle(),
            ),
            SyntacticPrimarySolver::Group(_) => todo!(),
            SyntacticPrimarySolver::MetaIdentifier(_) => todo!(),
            SyntacticPrimarySolver::TerminalString(_) => todo!(),
            SyntacticPrimarySolver::SpecialSequence(_) => todo!(),
            SyntacticPrimarySolver::EmptySequence => todo!(),
        }
    }
}

#[test]
pub fn test_basic_ebnf_solver_only_definition() {
    let solver =
        EbnfSolver::from_syntax(crate::parser::parse_ebnf("letter='hello';", true).unwrap());
    assert!(solver.solve("hello"));
}

#[test]
pub fn test_basic_ebnf_solver_with_or() {
    let solver = EbnfSolver::from_syntax(
        crate::parser::parse_ebnf("letter='hello' | 'world';", true).unwrap(),
    );
    assert!(solver.solve("hello"));
    assert!(solver.solve("world"));
}

#[test]
pub fn test_basic_ebnf_solver_with_meta_ident() {
    let solver = EbnfSolver::from_syntax(
        crate::parser::parse_ebnf(
            r##"
        hello = 'hello';
        world = 'world';
        letter = hello | world;"##,
            true,
        )
        .unwrap(),
    );
    assert!(solver.solve("hello"));
    assert!(solver.solve("world"));
}

#[test]
pub fn test_basic_ebnf_solver_with_concat() {
    let solver = EbnfSolver::from_syntax(
        crate::parser::parse_ebnf(
            r##"
        hello = 'hello';
        world = 'world';
        letter = hello , world;"##,
            true,
        )
        .unwrap(),
    );
    assert!(solver.solve("helloworld"));
}

#[test]
pub fn test_basic_ebnf_solver_with_concat_and_repeat() {
    let solver = EbnfSolver::from_syntax(
        crate::parser::parse_ebnf(
            r##"
        hello = 'hello';
        world = 'world';
        letter = {hello} , {world};"##,
            true,
        )
        .unwrap(),
    );
    assert!(solver.solve("hellohelloworldworld"));
    assert!(solver.solve("worldworld"));
    assert!(solver.solve("helloworldhelloworld"));
}

#[test]
pub fn test_parse_ebnf_itself() {
    let solver = EbnfSolver::from_syntax(
        crate::parser::parse_ebnf(
            r##"
            letter='a'|'b'|'c'|'d';
            decimaldigit='1'|'2'|'3'|;
            syntax=syntaxrule,{syntaxrule};
            syntaxrule=metaidentifier,'=',definitionslist,';';
            definitionslist=singledefinition,{'|',singledefinition};
            singledefinition=term,{',',term};term=factor,['-',exception];
            exception=factor;
            factor=[integer,'*'],primary;
            primary=optionalsequence|repeatedsequence|specialsequence|groupedsequence|metaidentifier|terminalstring|empty;
            empty=;
            optionalsequence='[',definitionslist,']';
            repeatedsequence='{',definitionslist,'}';
            groupedsequence='(',definitionslist,')';
            terminalstring="'",character-"'",{character-"'"},"'"|'"',character-'"',{character-'"'},'"';
            metaidentifier=letter,{letter|decimaldigit};
            integer=decimaldigit,{decimaldigit};
            specialsequence='?',{character-'?'},'?';
            comment='(*',{commentsymbol},'*)';
            commentsymbol=comment|terminalstring|specialsequence|character;"##,
            true,
        )
        .unwrap(),
    );
    println!("{:#?}", solver);
    assert!(solver.solve(
        r##"
    letter='a'|'b'|'c'|'d';
    syntax=syntaxrule,{syntaxrule};
    syntaxrule=metaidentifier,'=',definitionslist,';';
    definitionslist=singledefinition,{'|',singledefinition};
    singledefinition=term,{',',term};term=factor,['-',exception];
    exception=factor;
    factor=[integer,'*'],primary;
    primary=optionalsequence|repeatedsequence|specialsequence|groupedsequence|metaidentifier|terminalstring|empty;
    empty=;
    optionalsequence='[',definitionslist,']';
    repeatedsequence='{',definitionslist,'}';
    groupedsequence='(',definitionslist,')';
    terminalstring="'",character-"'",{character-"'"},"'"|'"',character-'"',{character-'"'},'"';
    metaidentifier=letter,{letter|decimaldigit};
    integer=decimaldigit,{decimaldigit};
    specialsequence='?',{character-'?'},'?';
    comment='(*',{commentsymbol},'*)';
    commentsymbol=comment|terminalstring|specialsequence|character;"##));
}
