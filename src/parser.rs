use std::fmt::{Debug, Display, Write};

use crate::{Lexer, Token, TokenType};

struct Parser {
    lexer: Lexer,
    current_token: Token,
    peek_token: Token, // I need a peek token because the lexer throws away tokens as the next one
                       // is read
}

impl Parser {
    fn advance_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    fn parse_statement(&mut self) -> Result<Statement, &str> {
        match self.current_token.token_type {
            TokenType::LET => Ok(Statement::LetStatement(self.parse_let_statement()?)), // TODO: Fix ?
            TokenType::RETURN => Ok(Statement::ReturnStatement(self.parse_return_statement())),
            _ => Ok(Statement::ExpressionStatement(
                self.parse_expression_statement()?,
            )),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<ExpressionStatement, &str> {
        let expression_token = self.current_token.clone();
        let expression = self
            .parse_expression(0)
            .expect(".parse_expression() is None"); // todo: LOWEST
                                                    // instead of 0

        // purge semicolons off the end of statements
        if self.peek_token.token_type == TokenType::SEMICOLON {
            self.advance_token();
        }

        return Ok(ExpressionStatement {
            expr: expression,
            token: expression_token,
        });
    }

    fn parse_boolean(&mut self) -> BooleanLiteral {
        return BooleanLiteral {
            token: self.current_token.clone(),
            value: self.curr_token_is(TokenType::TRUE),
        };
    }

    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.advance_token();

        let expr = self.parse_expression(0); //TODO: Replace 0
        if !self.expect_peek(TokenType::RPARAN) {
            return None;
        }

        return expr;
    }

    fn parse_expression(&mut self, precedence: i32) -> Option<Expression> {
        let prefix = match &self.current_token.token_type {
            TokenType::IDENTIFIER => Some(Expression::Identifer(self.parse_identifier())),
            TokenType::INT => Some(Expression::IntegerLiteral(
                self.parse_integer_literal().expect("WTF WTF?!?!?"),
            )), // TODO: change to Option? or change all to Result
            TokenType::MINUS | TokenType::BANG => Some(Expression::PrefixExpression(
                self.parse_prefix_expression()?,
            )),
            TokenType::TRUE | TokenType::FALSE => {
                Some(Expression::BooleanLiteral(self.parse_boolean()))
            }
            TokenType::LPARAN => self.parse_grouped_expression(),
            _ => None,
        };

        let mut expr = prefix;

        // while there's a
        while (!self.peek_token_is(TokenType::SEMICOLON))
            && (precedence < self.peek_token_precedence())
        // keeps going until end of statement or
        // finds a token of higher precedence?
        {
            if [
                TokenType::PLUS,
                TokenType::MINUS,
                TokenType::ASTERISK,
                TokenType::SLASH,
                TokenType::GT,
                TokenType::LT,
                TokenType::EQ,
                TokenType::NOT_EQ,
            ]
            .contains(&self.peek_token.token_type)
            {
                self.advance_token();
                expr = Some(Expression::InfixExpression(
                    self.parse_infix_expression(expr?)?,
                ));
            } else {
                break; // I think this is important, replaces the return they had
            }
        }
        return Some(expr?);
    }

    fn parse_let_statement(&mut self) -> Result<LetStatement, &str> {
        let let_token = self.current_token.clone();

        // read identifer
        if !self.expect_peek(TokenType::IDENTIFIER) {
            return Err("expected identifier");
        }
        let identifer = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        // read assignment
        if !self.expect_peek(TokenType::ASSIGN) {
            return Err("expected =");
        }

        println!("\t\t = read");

        //TODO: Expressions, currently I'm skipping until I hit a semi colon
        while !self.curr_token_is(TokenType::SEMICOLON) {
            println!("SKIPPED");
            self.advance_token();
        }

        return Ok(LetStatement {
            name: identifer,
            token: let_token,
        });
    }

    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token.token_type == t {
            self.advance_token();
            return true;
        }
        return false;
    }

    fn curr_token_is(&mut self, t: TokenType) -> bool {
        return self.current_token.token_type == t;
    }

    fn curr_token_precedence(&self) -> i32 {
        self.current_token.token_type.precedence()
    }

    fn peek_token_is(&mut self, t: TokenType) -> bool {
        return self.peek_token.token_type == t;
    }

    fn peek_token_precedence(&self) -> i32 {
        self.peek_token.token_type.precedence()
    }

    fn parse_return_statement(&mut self) -> ReturnStatement {
        let return_token = self.current_token.clone();
        let expression = self.parse_expression(0); // TODO: figure out precedence
        self.advance_token();

        /*let expression = match expression {
            Some(expr) => expr,
            _ => panic!("return statement parsing error, {:?}", expression),
        };*/
        let expression = Expression::Identifer(Identifier {
            token: self.current_token.clone(),
            value: "just for testing".to_string(),
        }); // This is here so it even runs

        //TODO: handle expression, currently skipping until I hit one
        while !self.curr_token_is(TokenType::SEMICOLON) {
            self.advance_token();
        }

        return ReturnStatement {
            return_value: expression,
            token: return_token,
        };
    }

    // recursive descent parsing
    fn parse(&mut self) -> Vec<Statement> {
        let mut statements: Vec<Statement> = vec![];
        let mut errors: Vec<String> = vec![];

        while !self.curr_token_is(TokenType::EOF) {
            println!("\nNEW STATEMENT token: {:?}", self.current_token);
            let statement = self.parse_statement();
            match statement {
                Ok(statement) => statements.push(statement),
                Err(str) => errors.push(str.to_string()),
            }

            self.advance_token()
        }

        return statements;
    }

    fn parse_identifier(&mut self) -> Identifier {
        return Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };
    }

    // skipped the error handling
    fn parse_integer_literal(&mut self) -> Result<IntegerLiteral, std::num::ParseIntError> {
        let val: i64 = self.current_token.literal.parse()?;
        return Ok(IntegerLiteral {
            token: self.current_token.clone(),
            value: val,
        });
    }

    // Used for BANG and MINUS
    fn parse_prefix_expression(&mut self) -> Option<PrefixExpression> {
        //TODO: Take a hard look at
        //this?
        if ![TokenType::MINUS, TokenType::BANG].contains(&self.current_token.token_type) {
            return None; // TODO
        }

        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        self.advance_token();
        let right = self.parse_expression(5); // TODO: Replace 6 it represents PREFIX I think
        return Some(PrefixExpression {
            token,
            operator,
            right: Box::new(right?), //TODO: Should this be ?
        });
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<InfixExpression> {
        let token = self.current_token.clone();
        let operator = self.current_token.literal.clone();
        let precedence = self.curr_token_precedence();

        self.advance_token();
        let right = self.parse_expression(precedence);

        return Some(InfixExpression {
            token,
            operator,
            left: Box::new(left),
            right: Box::new(right?),
        });
    }

    fn new(mut lexer: Lexer) -> Parser {
        let current_token = lexer.next_token(); // pulls the first token off
        let peek_token = lexer.next_token();

        let p = Parser {
            lexer,
            current_token,
            peek_token,
        };

        return p;
    }
}

#[derive(Debug)]
enum Statement {
    LetStatement(LetStatement),
    ReturnStatement(ReturnStatement),
    ExpressionStatement(ExpressionStatement),
}

impl Statement {
    fn token_literal(&self) -> &Token {
        match self {
            Statement::LetStatement(let_statement) => &let_statement.token,
            Statement::ReturnStatement(return_statement) => &return_statement.token,
            Statement::ExpressionStatement(expression_statement) => &expression_statement.token,
        }
    }

    //fn statement_node()
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::LetStatement(let_statement) => {
                write!(f, "let {} =", let_statement.name.value)
            }
            Statement::ReturnStatement(return_statement) => write!(f, "return"),
            Statement::ExpressionStatement(expression_statement) => {
                write!(f, "{}", expression_statement.expr)
            }
        }
    }
}

// Statement structs
#[derive(Debug)]
struct LetStatement {
    name: Identifier,
    token: Token,
}

#[derive(Debug)]
struct ReturnStatement {
    return_value: Expression,
    token: Token,
}

#[derive(Debug)]
struct ExpressionStatement {
    expr: Expression,
    token: Token, // first token of expression
}

// what are those?

// expression structs
#[derive(Debug, Clone)]
enum Expression {
    Identifer(Identifier),
    IntegerLiteral(IntegerLiteral),
    BooleanLiteral(BooleanLiteral),
    StringLiteral(StringLiteral),
    PrefixExpression(PrefixExpression),
    InfixExpression(InfixExpression),
}

impl Expression {
    fn token_literal(&self) -> &Token {
        match self {
            Expression::Identifer(identifier) => &identifier.token,
            Expression::IntegerLiteral(integer_literal) => &integer_literal.token,
            Expression::BooleanLiteral(boolean_literal) => &boolean_literal.token,
            Expression::StringLiteral(string_literal) => &string_literal.token,
            Expression::PrefixExpression(prefix_expression) => &prefix_expression.token,
            Expression::InfixExpression(infix_expression) => &infix_expression.token,
        }
    }

    // fn expression_node
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifer(identifier) => write!(f, "{}", identifier.value),
            Expression::IntegerLiteral(integer_literal) => write!(f, "{}", integer_literal.value),
            Expression::BooleanLiteral(boolean_literal) => write!(f, "{}", boolean_literal.value),
            Expression::StringLiteral(string_literal) => write!(f, "{}", string_literal.value),
            Expression::PrefixExpression(prefix) => {
                write!(f, "({}{})", prefix.operator, prefix.right)
            }
            Expression::InfixExpression(infix) => {
                write!(f, "({} {} {})", infix.left, infix.operator, infix.right)
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Identifier {
    token: Token,
    value: String,
}

#[derive(Debug, Clone)]
struct IntegerLiteral {
    token: Token,
    value: i64,
}

#[derive(Debug, Clone)]
struct BooleanLiteral {
    token: Token,
    value: bool,
}

#[derive(Debug, Clone)]
struct StringLiteral {
    token: Token,
    value: String,
}

#[derive(Debug, Clone)]
struct PrefixExpression {
    token: Token,
    operator: String,
    right: Box<Expression>, //TODO: what is this box doing?
}

#[derive(Debug, Clone)]
struct InfixExpression {
    token: Token,
    operator: String,
    left: Box<Expression>,
    right: Box<Expression>,
}

#[test]
fn test_return_statement() {
    let input = "return 5;
    return 10;
    return 993322;";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let statements = p.parse();
    //TODO: checkParseErrors(p);??

    assert_eq!(statements.len(), 3);

    for s in statements {
        match s {
            Statement::ReturnStatement(..) => {}
            _ => panic!("type mismatch: {:?} expected ReturnStatement", s),
        }
    }
}

#[test]
fn test_return_statement_identifier() {
    let input = "return x;
    return y;
    return zzz;";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let statements = p.parse();
    //TODO: checkParseErrors(p);??

    assert_eq!(statements.len(), 3);

    for s in statements {
        match s {
            Statement::ReturnStatement(..) => {}
            _ => panic!("type mismatch: {:?} expected ReturnStatement", s),
        }
    }
}

#[test]
fn test_let() {
    let input = "let x = 12;";
    let l = Lexer::new(input.to_string());
    let mut p = Parser::new(l);

    let statements = p.parse();
    //TODO: checkParseErrors(p);??

    assert_eq!(statements.len(), 1);

    for s in statements {
        match s {
            Statement::LetStatement(..) => {}
            _ => panic!("type mismatch: {:?} expected ReturnStatement", s),
        }
    }
}

#[test]
fn test_precedence_parsing() {
    let tests = [
        ("true", "true"),
        ("false", "false"),
        ("3 > 5 == false", "((3 > 5) == false)"),
        ("3 < 5 == true", "((3 < 5) == true)"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("-a*b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
        ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
        ("(5 + 5) * 2", "((5 + 5) * 2)"),
        ("2 / (5 + 5)", "(2 / (5 + 5))"),
        ("-(5 + 5)", "(-(5 + 5))"),
        ("!(true == true)", "(!(true == true))"),
    ];

    for test in tests {
        let l = Lexer::new(test.0.to_string());
        let mut p = Parser::new(l);

        let statements = p.parse();

        // This is how program struct need to .to_string()
        let mut output = String::from("");
        for s in statements {
            output += &s.to_string()
        }

        assert_eq!(output, test.1);
    }
}

/*
fn test_identifier(iden: Identifier, val: String) -> bool {
    iden.value == val
}

fn test_integer_literal(int: IntegerLiteral, val: i64) -> bool {
    int.value == val
}
*/
// Do I'm not sure what they want be to do in Rust, they use casts of some kind in go to change
// the input values type to match a type that working with each test case?
fn test_literal_expression_value(expr: Expression, expected_value: &str) -> bool {
    match expr {
        Expression::IntegerLiteral(literal) => {
            return literal.value == expected_value.parse().expect("hardcoded test value")
        }
        Expression::Identifer(literal) => {
            return literal.value == expected_value;
        }
        Expression::BooleanLiteral(literal) => {
            return literal.value == expected_value.parse().expect("hardcoded test value")
        }
        _ => false, // not a literal expression
    }
}

fn test_infix_expression_values(
    expr: Expression,
    expected_left: &str,
    expected_operator: &str,
    expected_right: &str,
) -> bool {
    match expr {
        Expression::InfixExpression(infix_expr) => {
            assert!(test_literal_expression_value(
                *infix_expr.left,
                expected_left
            ));
            assert!(infix_expr.operator == expected_operator);
            assert!(test_literal_expression_value(
                *infix_expr.right,
                expected_right
            ));
            return true;
        }
        _ => false, // not an infix expression, make a result and throw an Err instead
    }
}

/* NOTE: I don't think I need it because I could just use test_literal_expression_value which
 * contains everything that would be here except for the .TokenLiteral() check
fn test_boolean_literal(expr: Expression, expected: bool) -> bool {
    match expr {
        Expression::BooleanLiteral(l) => test_literal_expression_value(l.value, expected.to_string()),
        _ => false,
    }
}
*/

#[test]
fn test_parsing_infix_expressions() {
    let tests = [
        ("true == true", "true", "==", "true"),
        ("true != false", "true", "!=", "false"),
        ("false == false", "false", "==", "false"),
    ];

    for test in tests {
        let l = Lexer::new(test.0.to_string());
        let mut p = Parser::new(l);
        let expr_statement = &p.parse()[0];
        match expr_statement {
            Statement::ExpressionStatement(expr) => {
                assert!(test_infix_expression_values(
                    expr.expr.clone(),
                    test.1,
                    test.2,
                    test.3
                ))
            }
            _ => assert!(false),
        }
    }
}
