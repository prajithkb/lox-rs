use std::fmt::Display;

use crate::tokens::{Literal, Token};

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(Box<Expr>, Token, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Option<Literal>),
    Unary(Token, Box<Expr>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&to_string(self))
    }
}
fn literal_to_str(literal: &Option<Literal>) -> String {
    match literal {
        Some(v) => v.to_string(),
        None => "nil".to_string(),
    }
}

fn to_string(expr: &Expr) -> String {
    match expr {
        Expr::Binary(left, operator, right) => parenthesize(&operator.lexeme, &[left, right]),
        Expr::Grouping(expr) => parenthesize("Grouping", &[expr]),
        Expr::Literal(literal) => parenthesize(&literal_to_str(literal), &[]),
        Expr::Unary(operator, expr) => parenthesize(&operator.lexeme, &[expr]),
    }
}

fn parenthesize(name: &str, exprs: &[&Box<Expr>]) -> String {
    let mut result = String::new();
    result.push('(');
    result.push_str(name);
    for &expr in exprs {
        result.push(' ');
        result.push_str(&to_string(expr))
    }
    result.push(')');
    result
}

#[cfg(test)]
mod tests {
    use crate::tokens::{Literal, Token, TokenType};

    use super::Expr;

    #[test]
    fn creates_ast_correctly() {
        let expr = Expr::Binary(
            Box::new(Expr::Unary(
                Token::new(
                    TokenType::Minus,
                    "-".to_string(),
                    0,
                    Literal::opt_number(1.0),
                ),
                Box::new(Expr::Literal(Literal::opt_number(123.0))),
            )),
            Token::new(
                TokenType::Star,
                "*".to_string(),
                0,
                Literal::opt_number(1.0),
            ),
            Box::new(Expr::Grouping(Box::new(Expr::Literal(
                Literal::opt_number(45.67),
            )))),
        );
        assert_eq!(
            "(* (- (Number(123.0))) (Grouping (Number(45.67))))",
            expr.to_string()
        );
    }
}
