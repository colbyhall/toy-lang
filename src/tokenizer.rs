use std::iter::Iterator;

#[derive(Debug)]
pub enum Keyword {
    If,
    Let,
    While,
    For,
    Loop,
    Fn,
    Struct,
}

#[derive(Debug)]
pub enum Seperator {
    LeftCurly,
    RightCurly,
    LeftBracket,
    RightBracket,
    OpenParenthesis,
    CloseParenthesis,
    Colon,
    SemiColon,
    Comma,
    Period,
}

impl Seperator {
    pub fn is(c: char) -> bool {
        matches!(c, '{' | '}' | '[' | ']' | '(' | ')' | ':' | ';' | ',' | '.')
    }
}

#[derive(Debug)]
pub enum Operator {
    Equal,
    LessThan,
    GreaterThan,
    Bang,
    Backslack,
    Plus,
    Minus,
    Asterisk,
    Arrow,
}

impl Operator {
    pub fn is(c: char) -> bool {
        matches!(c, '=' | '<' | '>' | '!' | '/' | '+' | '-' | '*')
    }
}

#[derive(Debug)]
pub enum Number {
    Float(f64),
    Int(u64),
}

#[derive(Debug)]
pub enum Literal<'a> {
    Char(char),
    String(&'a str),
    Number(Number),
}

#[derive(Debug)]
pub enum Token<'a> {
    Identifier(&'a str),
    Keyword(Keyword),
    Seperator(Seperator),
    Operator(Operator),
    Literal(Literal<'a>),
    Comment,
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    contents: &'a str,

    line: i32,
    col: i32,
}

impl<'a> Tokenizer<'a> {
    pub fn new(contents: &'a str) -> Self {
        Self {
            contents,

            line: 1,
            col: 1,
        }
    }

    fn eat_multiline_comment_recursive(&mut self) -> bool {
        let len = self.contents.len();
        self.contents = &self.contents["/*".len()..len];

        loop {
            let len = self.contents.len();
            if self.contents.starts_with("/*") {
                if self.eat_multiline_comment_recursive() {
                    return true;
                }
            } else if self.contents.starts_with("*/") {
                self.contents = &self.contents["*/".len()..len];
                break;
            }

            let len = self.contents.len();
            if let Some(c) = self.contents.chars().next() {
                self.contents = &self.contents[c.len_utf8()..len];
            } else {
                return true;
            }
        }
        false
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let len = self.contents.len();

        fn is_ident(c: char) -> bool {
            c.is_alphanumeric() || c == '_'
        }

        let c = self.contents.chars().next()?;
        self.col += 1;
        if c.is_whitespace() {
            match c {
                '\n' => self.line += 1,
                '\r' => self.col = 1,
                _ => {}
            }
            self.contents = &self.contents[c.len_utf8()..len];
            return self.next();
        }
        // Identifier/Keyword tokenization
        else if c.is_alphabetic() || c == '_' {
            let last = self
                .contents
                .chars()
                .enumerate()
                .find(|(_, c)| !is_ident(*c))
                .map(|(i, _)| i)
                .unwrap_or(len);

            let ident = &self.contents[0..last];
            self.contents = &self.contents[last..len];

            // Keyword matching
            return Some(match ident {
                "if" => Token::Keyword(Keyword::If),
                "let" => Token::Keyword(Keyword::Let),
                "while" => Token::Keyword(Keyword::While),
                "for" => Token::Keyword(Keyword::For),
                "loop" => Token::Keyword(Keyword::Loop),
                "fn" => Token::Keyword(Keyword::Fn),
                "struct" => Token::Keyword(Keyword::Struct),
                _ => Token::Identifier(ident),
            });
        }

        if c.is_numeric() {
            if self.contents.starts_with("0x") {
                todo!()
            } else {
                let mut index = 0;
                let mut decimal_point = false;
                for (i, c) in self.contents.chars().enumerate() {
                    if c == '.' {
                        if decimal_point {
                            todo!("Error Handling");
                        }
                        decimal_point = true;
                    }

                    if !(c.is_numeric() || i == 0 && c == '-') {
                        index = i;
                        break;
                    }
                }

                let number = &self.contents[0..index];
                let number = if decimal_point {
                    if let Ok(value) = number.parse::<f64>() {
                        Number::Float(value)
                    } else {
                        todo!("Error Handling");
                    }
                } else if let Ok(value) = number.parse::<u64>() {
                    Number::Int(value)
                } else {
                    todo!("Error handling")
                };

                self.contents = &self.contents[index..len];

                return Some(Token::Literal(Literal::Number(number)));
            }
        } else if c == '\'' {
            // Char
            if let Some(value) = self.contents.chars().nth(1) {
                if let Some(end) = self.contents.chars().nth(2) {
                    if end == '\'' {
                        self.contents = &self.contents[2 + value.len_utf8()..len];
                        return Some(Token::Literal(Literal::Char(value)));
                    } else {
                        todo!("Error Handling");
                    }
                } else {
                    todo!("Error Handling");
                }
            } else {
                todo!("Error Handling");
            }
        } else if c == '\"' {
            // Strings
            let sub = &self.contents[1..len];
            let len = sub.len();
            if let Some((end, _)) = sub.chars().enumerate().find(|(_, c)| *c == '"') {
                let string = &sub[0..end];
                self.contents = &sub[end + 1..len];
                return Some(Token::Literal(Literal::String(string)));
            } else {
                todo!("Error Handling");
            }
        }

        // Seperators, Operators, and Comments
        if Operator::is(c) || Seperator::is(c) {
            if c == '/' {
                if let Some(next) = self.contents.chars().nth(1) {
                    if next == '/' {
                        if let Some((advance, _)) =
                            self.contents.chars().enumerate().find(|(_, c)| *c == '\n')
                        {
                            self.contents = &self.contents[advance..len];
                        }

                        Some(Token::Comment)
                    } else if next == '*' {
                        self.eat_multiline_comment_recursive();
                        Some(Token::Comment)
                    } else {
                        Some(Token::Operator(Operator::Backslack))
                    }
                } else {
                    // EOF
                    Some(Token::Operator(Operator::Backslack))
                }
            } else if c == '-' {
                let token = if let Some(next) = self.contents.chars().nth(1) {
                    if next == '>' {
                        self.contents = &self.contents["->".len()..len];
                        Token::Operator(Operator::Arrow)
                    } else {
                        self.contents = &self.contents[c.len_utf8()..len];
                        Token::Operator(Operator::Minus)
                    }
                } else {
                    self.contents = &self.contents[c.len_utf8()..len];
                    Token::Operator(Operator::Minus)
                };
                Some(token)
            } else {
                let token = match c {
                    '{' => Token::Seperator(Seperator::LeftCurly),
                    '}' => Token::Seperator(Seperator::RightCurly),
                    '[' => Token::Seperator(Seperator::LeftBracket),
                    ']' => Token::Seperator(Seperator::RightBracket),
                    '(' => Token::Seperator(Seperator::OpenParenthesis),
                    ')' => Token::Seperator(Seperator::CloseParenthesis),
                    ':' => Token::Seperator(Seperator::Colon),
                    ';' => Token::Seperator(Seperator::SemiColon),
                    ',' => Token::Seperator(Seperator::Comma),
                    '.' => Token::Seperator(Seperator::Period),
                    '=' => Token::Operator(Operator::Equal),
                    '<' => Token::Operator(Operator::LessThan),
                    '>' => Token::Operator(Operator::GreaterThan),
                    '!' => Token::Operator(Operator::Bang),
                    '/' => Token::Operator(Operator::Backslack),
                    '+' => Token::Operator(Operator::Plus),
                    '*' => Token::Operator(Operator::Asterisk),
                    _ => todo!(),
                };
                self.contents = &self.contents[c.len_utf8()..len];
                Some(token)
            }
        } else {
            unreachable!()
        }
    }
}
