use std::{collections::HashMap, ops::Add};

use crate::{
    parser::parse_ebnf, DefinitionList, EbnfSyntax, SingleDefinition, SyntacticFactor,
    SyntacticPrimary, SyntacticTerm,
};

pub struct EbnfSolver {
    rules: HashMap<String, DefinitionList>,
}

trait Satisfy {
    fn satisfy(&self, context: &EbnfSolver, input: &str) -> String;
}

impl EbnfSolver {
    pub fn from_syntax(syntax: EbnfSyntax) -> Self {
        Self {
            rules: syntax
                .rules
                .into_iter()
                .map(|v| (v.name, v.definition))
                .collect(),
        }
    }

    pub fn solve(&self, input: &str) -> bool {
        let mut result = String::with_capacity(input.len());
        loop {
            let search = &input[result.len()..];
            if search.is_empty() {
                break;
            }
            let generated = self.satisfy(&self, search);
            if generated.is_empty() {
                break;
            }
            result = result.add(&generated);
        }
        input == result
    }
}

impl Satisfy for EbnfSolver {
    fn satisfy(&self, context: &EbnfSolver, input: &str) -> String {
        self.rules
            .values()
            .map(|v| v.satisfy(&context, input))
            .max()
            .unwrap_or_default()
    }
}

impl Satisfy for DefinitionList {
    fn satisfy(&self, context: &EbnfSolver, input: &str) -> String {
        self.definitions
            .iter()
            .map(|v| v.satisfy(context, input))
            .max()
            .unwrap_or_default()
    }
}

impl Satisfy for SingleDefinition {
    fn satisfy(&self, context: &EbnfSolver, input: &str) -> String {
        self.terms
            .iter()
            .map(|v| v.satisfy(context, input))
            .collect()
    }
}

impl Satisfy for SyntacticTerm {
    fn satisfy(&self, context: &EbnfSolver, input: &str) -> String {
        self.factor.satisfy(context, input)
    }
}

impl Satisfy for SyntacticFactor {
    fn satisfy(&self, context: &EbnfSolver, input: &str) -> String {
        self.primary.satisfy(context, input).repeat(self.count)
    }
}

impl Satisfy for SyntacticPrimary {
    fn satisfy(&self, context: &EbnfSolver, input: &str) -> String {
        match self {
            SyntacticPrimary::Optional(_) => todo!(),
            SyntacticPrimary::Repeat(_) => todo!(),
            SyntacticPrimary::Group(_) => todo!(),
            SyntacticPrimary::MetaIdentifier(s) => context
                .rules
                .get(s)
                .map(|v| v.satisfy(context, input))
                .unwrap_or_default(),
            SyntacticPrimary::TerminalString(s) => {
                let largest_idx_match = input
                    .chars()
                    .zip(s.chars())
                    .enumerate()
                    .find_map(|(idx, (l, r))| if l != r { Some(idx) } else { None })
                    .unwrap_or(s.len().min(input.len()));
                s[..dbg!(largest_idx_match)].to_string()
            }
            SyntacticPrimary::SpecialSequence(_) => todo!(),
            SyntacticPrimary::EmptySequence => todo!(),
        }
    }
}

#[test]
pub fn test_basic_ebnf_solver_only_definition() {
    let solver = EbnfSolver::from_syntax(parse_ebnf("letter='hello';", true).unwrap());
    assert!(solver.solve("hello"));
}

#[test]
pub fn test_basic_ebnf_solver_with_or() {
    let solver = EbnfSolver::from_syntax(parse_ebnf("letter='hello' | 'world';", true).unwrap());
    assert!(solver.solve("hello"));
    assert!(solver.solve("world"));
}

#[test]
pub fn test_basic_ebnf_solver_with_meta_ident() {
    let solver = EbnfSolver::from_syntax(
        parse_ebnf(
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
        parse_ebnf(
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
        parse_ebnf(
            r##"
        hello = 'hello';
        world = 'world';
        letter = {hello} , {world};"##,
            true,
        )
        .unwrap(),
    );
    assert!(solver.solve("hellohelloworldworld"));
}
