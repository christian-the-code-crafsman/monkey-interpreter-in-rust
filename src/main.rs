use core::fmt;
use std::collections::HashMap;
use std::io::stdin;
use std::io::stdout;
use std::io::Write;

#[derive(Clone, Debug, Eq, PartialEq)]
enum TokenType {
    ILLEGAL, // represents token we don't know how to parse
    EOF,     // represents end of the source file

    /* identifiers + literals */
    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    /* delimiters */
    COMMA,
    SEMICOLON,

    LPARAN,
    RPARAN,
    LBRACE,
    RBRACE,

    /* keyword */
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,

    /* identifer */
    IDENTIFIER,

    /* numbers */
    INT,

    /* two character tokens*/
    EQ,
    NOT_EQ,
}

#[derive(Debug)]
struct Token {
    token_type: TokenType,
    literal: String,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}, {}", self.token_type, self.literal)
    }
}

struct Lexer {
    input: String,
    read_position: usize,
    current_char: char,
}

//TODO: move maps and lists into constants??

impl Lexer {
    // Advances until a non-whitespace character or the end of input is found
    fn eat_whitespace(&mut self) {
        let whitespace_chars = [' ', '\t', '\n', '\r']; // whitespace characters defined in the
                                                        // Monkey language
        while whitespace_chars.contains(&self.current_char) {
            self.read_char();
        }
    }

    // creates a two character token, and advances to the character after it ends
    fn make_two_character_token(&mut self, token_type: TokenType) -> Token {
        let first_char = self.current_char;
        self.read_char();
        self.read_char();
        return Token {
            token_type,
            literal: String::from(first_char) + &String::from(self.current_char.clone()),
        };
    }

    // returns the next token, and advances the read position to the character after it ends
    fn next_token(&mut self) -> Token {
        self.eat_whitespace();

        // handle everything read
        if self.current_char == '\0' {
            return Token {
                literal: String::from(""),
                token_type: TokenType::EOF,
            };
        }

        let single_char_token_map = HashMap::from([
            ('=', TokenType::ASSIGN),
            ('(', TokenType::LPARAN),
            (')', TokenType::RPARAN),
            ('{', TokenType::LBRACE),
            ('}', TokenType::RBRACE),
            (',', TokenType::COMMA),
            (';', TokenType::SEMICOLON),
            ('+', TokenType::PLUS),
            ('-', TokenType::MINUS),
            ('!', TokenType::BANG),
            ('*', TokenType::ASTERISK),
            ('/', TokenType::SLASH),
            ('>', TokenType::GT),
            ('<', TokenType::LT),
            ('\0', TokenType::EOF),
        ]);

        let token_type = match single_char_token_map.get(&self.current_char) {
            Some(t) => {
                // I don't like this, I should compare one character than another, the shortcutting
                // on the && so I don't unnecessarily peek characters is nice though. PROBLEM: I should only
                // be checking for a 2nd equal on the case where I already know the current
                // character is an equal. Here, I'm checking if the 1st character matches any of
                // the list, then checking if the character is a equal again, then if so checking
                // if the next character is an equal.
                if t == &TokenType::BANG && self.peek_char() == '=' {
                    return self.make_two_character_token(TokenType::NOT_EQ);
                } else if t == &TokenType::ASSIGN && self.peek_char() == '=' {
                    return self.make_two_character_token(TokenType::EQ);
                }
                t.clone()
            } // Clones whatever is pulled from the hashmap, this seems fine
            None => {
                // return early to avoid advancing an additional character
                if self.is_identifier_char() {
                    return self.read_identifier_token();
                } else if self.is_number_char() {
                    return self.read_number_token();
                }
                TokenType::ILLEGAL // no match found
            }
        };

        // store the current character I'm processing, then advance to the next character
        let current_char = self.current_char;
        self.read_char();

        return Token {
            token_type,
            literal: String::from(current_char),
        };
    }

    /* advances to the next character in the input? */
    fn read_char(&mut self) {
        self.current_char = self.input.chars().nth(self.read_position).unwrap_or('\0'); // '\0' represents all of input has been read
        self.read_position += 1;
    }

    /* return the next the next character without advancing */
    fn peek_char(&self) -> char {
        self.input.chars().nth(self.read_position).unwrap_or('\0') // '\0' represents all of input has been read
    }

    // side effect: advances the current_char and read_position to the end of the next identifier token
    // returns an identifier token, if the current_char is at the begginging of an identifier token
    fn read_identifier_token(&mut self) -> Token {
        let start_position = self.read_position - 1; // start from the position of the current
                                                     // character
        while self.is_identifier_char() {
            self.read_char();
        }

        let literal = self.input[start_position..self.read_position - 1].to_string();
        let token_type = Lexer::lookup_identifier(&literal);
        return Token {
            literal,
            token_type,
        };
    }

    // side effect: advances the current_char and read_position to the end of the next identifier token
    // returns a number token, if the current_char is at the beginning of a number token
    fn read_number_token(&mut self) -> Token {
        let start_position = self.read_position - 1;
        while self.is_number_char() {
            self.read_char();
        }

        let literal = self.input[start_position..self.read_position - 1].to_string();
        return Token {
            literal,
            token_type: TokenType::INT,
        };
    }

    // Constructor for Lexer
    fn new(input: String) -> Lexer {
        let mut l = Lexer {
            input,
            read_position: 0,
            current_char: 0 as char,
        };
        l.read_char(); // primes the current character of the lexer
        return l;
    }

    // Returns true, if the current character is a valid character in an indentifier token literal
    fn is_identifier_char(&self) -> bool {
        return self.current_char.is_alphabetic() || self.current_char == '_';
    }

    // Returns true, if the current character is a valid character in a number token literal
    fn is_number_char(&self) -> bool {
        return self.current_char.is_numeric();
    }

    // return keyword TokenType, if the keyword exists otherwise returns IDENTIFIER TokenType
    // checks keyword "table" to determine if identifier is a keyword?
    fn lookup_identifier(identifier: &String) -> TokenType {
        let token_type_words = [
            ("fn", TokenType::FUNCTION),
            ("let", TokenType::LET),
            ("if", TokenType::IF),
            ("else", TokenType::ELSE),
            ("true", TokenType::TRUE),
            ("false", TokenType::FALSE),
            ("return", TokenType::RETURN),
        ];
        for (token_word, token_type) in token_type_words.into_iter() {
            if identifier == token_word {
                return token_type;
            }
        }
        return TokenType::IDENTIFIER;
    }
}

fn main() {
    /* a simple REPL, outputs the tokens created by a line */
    loop {
        print!(">>");
        stdout().flush().unwrap();
        let mut s = String::new();
        stdin().read_line(&mut s).unwrap();
        let mut l = Lexer::new(s);

        let mut t = l.next_token();
        while t.token_type != TokenType::EOF {
            println!("{}", t);
            t = l.next_token();
        }
    }
}

/* A helper function for test, exports all tokens from a string as a vector */
fn all_tokens_types_from_string(input: String) -> Vec<TokenType> {
    let mut l = Lexer::new(input);
    let mut tokens = vec![];
    loop {
        tokens.push(l.next_token());
        if tokens.iter().last().unwrap().token_type == TokenType::EOF {
            // I put something in so I know I have atleast 1 thing to unwrap
            break;
        }
    }

    // TODO: Figure out how to use a map to seperate out of the token types
    let mut token_types = vec![];
    for token in tokens.iter() {
        token_types.push(token.token_type.clone());
    }

    return token_types;
}

#[test]
fn lex_compound() {
    let correct_token_types = [
        TokenType::FUNCTION,
        TokenType::IDENTIFIER,
        TokenType::LPARAN,
        TokenType::RPARAN,
        TokenType::LBRACE,
        TokenType::LET,
        TokenType::IDENTIFIER,
        TokenType::ASSIGN,
        TokenType::LPARAN,
        TokenType::INT,
        TokenType::PLUS,
        TokenType::INT,
        TokenType::RPARAN,
        TokenType::SEMICOLON,
        TokenType::RBRACE,
        TokenType::EOF,
    ];

    let mut l = Lexer::new(String::from("fn main() { let i = (\n2 +  2    ); }"));

    for correct_token in correct_token_types.into_iter() {
        assert_eq!(l.next_token().token_type, correct_token);
    }
}

#[test]
fn two_char_tokens() {
    let token_types = all_tokens_types_from_string(String::from("!====!"));
    let token_types = dbg!(token_types);
    assert_eq!(
        token_types,
        vec![
            TokenType::NOT_EQ,
            TokenType::EQ,
            TokenType::ASSIGN,
            TokenType::BANG,
            TokenType::EOF
        ]
    );
}

/* TODO: Test case for whitespace between characters on two character tokens */
/* TODO: Test for illegal characters */
