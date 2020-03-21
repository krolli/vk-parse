#![allow(dead_code)]

//--------------------------------------------------------------------------------------------------
/// Performs Phase 1 of C compilation, specifically replaces platform-specific end-of-line indicators
/// with newlines characters and transforms trigraph sequences.
struct IterPhase1<'a> {
    src: &'a str,
}

impl<'a> IterPhase1<'a> {
    fn new(src: &'a str) -> Self {
        Self { src }
    }
}

impl<'a> Iterator for IterPhase1<'a> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        let mut iter = self.src.chars();

        let c = if let Some(c) = iter.next() {
            c
        } else {
            return None;
        };

        match c {
            // TODO: trigraphs and digraphs (probably not important)
            '\r' => {
                if let Some('\n') = iter.next() {
                    self.src = self.src.split_at(2).1;
                    Some('\n')
                } else {
                    self.src = self.src.split_at(1).1;
                    Some('\r')
                }
            }
            c => {
                self.src = self.src.split_at(1).1;
                Some(c)
            }
        }
    }
}

//--------------------------------------------------------------------------------------------------
struct IterPhase2<'a> {
    src: IterPhase1<'a>,
    c1: Option<char>,
    c2: Option<char>,
}

impl<'a> IterPhase2<'a> {
    fn new(code: &'a str) -> Self {
        let mut src = IterPhase1::new(code);
        let c1 = src.next();
        let c2 = src.next();
        Self { src, c1, c2 }
    }
}

impl<'a> Iterator for IterPhase2<'a> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        while let (Some('\\'), Some('\n')) = (self.c1, self.c2) {
            self.c1 = self.src.next();
            self.c2 = self.src.next();
        }

        let c = self.c1;
        self.c1 = self.c2;
        self.c2 = self.src.next();
        c
    }
}

//--------------------------------------------------------------------------------------------------
/// Part of phase 3 that deals with comments and whitespace. Does not produce tokens, just filters
/// and transforms characters.
struct IterPhase3a<'a> {
    src: IterPhase2<'a>,
    peek: Option<char>,
    prev_space: bool,
}

impl<'a> IterPhase3a<'a> {
    fn new(code: &'a str) -> Self {
        let mut src = IterPhase2::new(code);
        let peek = src.next();
        Self {
            src,
            peek,
            prev_space: false,
        }
    }

    fn is_merged_whitespace(c: char) -> bool {
        c == '\t' || ('\u{000B}' <= c && c <= '\u{000D}') || c == ' '
    }
}

impl<'a> Iterator for IterPhase3a<'a> {
    type Item = char;
    fn next(&mut self) -> Option<char> {
        loop {
            let c = if let Some(c) = self.peek {
                c
            } else {
                return None;
            };

            if c == '\n' {
                self.peek = self.src.next();
                self.prev_space = false;
                return Some('\n');
            } else if c == '/' {
                // line comment, block comment or just char
                self.peek = self.src.next();
                let c = if let Some(c) = self.peek {
                    c
                } else {
                    self.prev_space = false;
                    return Some('/');
                };

                if c == '/' {
                    self.peek = None;
                    while let Some(c) = self.src.next() {
                        if c == '\n' {
                            self.peek = Some(c);
                            break;
                        }
                    }
                    if !self.prev_space {
                        self.prev_space = true;
                        return Some(' ');
                    }
                } else if c == '*' {
                    while let Some(c) = self.src.next() {
                        if c == '*' {
                            if let Some('/') = self.src.next() {
                                self.peek = self.src.next();
                                break;
                            }
                        }
                    }
                    if !self.prev_space {
                        self.prev_space = true;
                        return Some(' ');
                    }
                } else {
                    self.prev_space = false;
                    return Some('/');
                }
            } else if IterPhase3a::is_merged_whitespace(c) {
                self.peek = None;
                while let Some(c) = self.src.next() {
                    if !IterPhase3a::is_merged_whitespace(c) {
                        self.peek = Some(c);
                        break;
                    }
                }
                if !self.prev_space {
                    self.prev_space = true;
                    return Some(' ');
                }
            } else {
                self.peek = self.src.next();
                self.prev_space = false;
                return Some(c);
            }
        }
    }
}

//--------------------------------------------------------------------------------------------------
/// Simplified tokens for easier dealing with most C code we care about. Not precisely according to
/// C/C++ standards, as those are not as easy to deal with and make more sense when combined with
/// preprocessor.
#[derive(Clone, Eq, PartialEq, Debug)]
enum Token<'a> {
    PpDirective(PpDirective),
    Punctuation,
    HeaderName(&'a str),
    Identifier(&'a str),
    StringLiteral(&'a str),
    CharacterLiteral(char),
    NewLine,
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum PpDirective {
    Define,
    Undef,
    Include,
    If,
    Ifdef,
    Ifndef,
    Else,
    Elif,
    Endif,
    Line,
    Error,
    Pragma,
}

impl PpDirective {
    fn from_str(s: &str) -> PpDirective {
        match s {
            "define" => PpDirective::Define,
            "undef" => PpDirective::Undef,
            "include" => PpDirective::Include,
            "if" => PpDirective::If,
            "ifdef" => PpDirective::Ifdef,
            "ifndef" => PpDirective::Ifndef,
            "else" => PpDirective::Else,
            "elif" => PpDirective::Elif,
            "endif" => PpDirective::Endif,
            "line" => PpDirective::Line,
            "error" => PpDirective::Error,
            "pragma" => PpDirective::Pragma,
            _ => panic!("Unrecognized postprocessor directive {:?}", s),
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum LineState {
    /// Right after newline.
    Start,
    /// When first thing encountered on the line is '#'
    PpStart,
    /// First identifier after '#'
    PpDirective,
    /// Contents of postprocessor directive.
    PpDirectiveBody,
    /// Reached newline but not reported yet.
    PpDirectiveEnd,
    /// Normal line tokens.
    Normal,
}

struct IterTokenInner<'src> {
    src: IterPhase3a<'src>,
    peek: Option<char>,
    line: LineState,
    buf: String,
}

impl<'src> IterTokenInner<'src> {
    fn new(src: &'src str) -> Self {
        let mut src = IterPhase3a::new(src);
        let peek = src.next();
        Self {
            src,
            peek,
            line: LineState::Start,
            buf: String::new(),
        }
    }

    fn next(&mut self) -> Option<Token> {
        self.buf.clear();
        loop {
            let c = if let Some(c) = self.peek {
                c
            } else {
                return None;
            };
            match (self.line, c) {
                (LineState::Start, ' ') => self.peek = self.src.next(),
                (LineState::Start, '\n') => self.peek = self.src.next(),
                (LineState::Start, '#') => {
                    self.line = LineState::PpStart;
                    self.peek = self.src.next();
                }
                (LineState::Start, c) => {
                    self.buf.push(c);
                    self.line = LineState::Normal;
                    self.peek = self.src.next();
                }

                (LineState::PpStart, ' ') => self.peek = self.src.next(),
                (LineState::PpStart, '\n') => panic!(
                    "Unexpected combination ({:?}, {:?})",
                    LineState::PpStart,
                    '\n'
                ),
                (LineState::PpStart, c) => {
                    self.buf.push(c);
                    self.line = LineState::PpDirective;
                    self.peek = self.src.next();
                }

                (LineState::PpDirective, ' ') => {
                    self.line = LineState::PpDirectiveBody;
                    self.peek = self.src.next();
                    return Some(Token::PpDirective(PpDirective::from_str(&self.buf)));
                }
                (LineState::PpDirective, '\n') => {
                    self.line = LineState::PpDirectiveEnd;
                    return Some(Token::PpDirective(PpDirective::from_str(&self.buf)));
                }
                (LineState::PpDirective, c) => {
                    self.buf.push(c);
                    self.peek = self.src.next();
                }

                (LineState::PpDirectiveBody, ' ') => {
                    self.peek = self.src.next();
                    return Some(Token::Identifier(&self.buf));
                }
                (LineState::PpDirectiveBody, '\n') => {
                    self.line = LineState::PpDirectiveEnd;
                    return Some(Token::Identifier(&self.buf));
                }
                (LineState::PpDirectiveBody, c) => {
                    self.buf.push(c);
                    self.peek = self.src.next();
                }

                (LineState::PpDirectiveEnd, '\n') => {
                    self.peek = self.src.next();
                    self.line = LineState::Start;
                    return Some(Token::NewLine);
                }
                (LineState::PpDirectiveEnd, c) => panic!(
                    "Unexpected combination ({:?}, {:?})",
                    LineState::PpDirectiveEnd,
                    c
                ),

                (LineState::Normal, ' ') => {
                    self.peek = self.src.next();
                    return Some(Token::Identifier(&self.buf));
                }
                (LineState::Normal, '\n') => {
                    self.peek = self.src.next();
                    self.line = LineState::Start;
                    if self.buf.len() > 0 {
                        return Some(Token::Identifier(&self.buf));
                    }
                }
                (LineState::Normal, ';') => {
                    if self.buf.len() > 0 {
                        return Some(Token::Identifier(&self.buf));
                    } else {
                        self.peek = self.src.next();
                        return Some(Token::Punctuation);
                    }
                }
                (LineState::Normal, c) => {
                    self.buf.push(c);
                    self.peek = self.src.next();
                }
            }
        }
    }
}

fn is_number_start(c: char) -> bool {
    '0' <= c && c <= '9'
}

fn is_number_part(c: char) -> bool {
    c == '.'
        || c == '+'
        || c == '-'
        || c == 'x'
        || c == 'X'
        || ('0' <= c && c <= '9')
        || ('a' <= c && c <= 'f')
        || ('A' <= c && c <= 'F')
}

fn is_identifier_start(c: char) -> bool {
    c == '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
}

fn is_identifier_part(c: char) -> bool {
    c == '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9')
}

//--------------------------------------------------------------------------------------------------
pub struct TokenIter<'a> {
    src: &'a str,
}

impl<'a> TokenIter<'a> {
    pub fn new(src: &'a str) -> Self {
        Self { src }
    }
}

impl<'a> Iterator for TokenIter<'a> {
    type Item = &'a str;
    fn next(&mut self) -> Option<&'a str> {
        let mut iter = self.src.char_indices();
        if let Some((_, c)) = iter.next() {
            if is_c_identifier_char(c) {
                for (end_idx, c) in iter {
                    if !is_c_identifier_char(c) {
                        let split = self.src.split_at(end_idx);
                        self.src = split.1;
                        return Some(split.0);
                    }
                }

                let res = self.src;
                self.src = "";
                return Some(res);
            } else {
                let split = self.src.split_at(1);
                self.src = split.1;
                return Some(split.0);
            }
        }

        None
    }
}

pub fn is_c_identifier_char(c: char) -> bool {
    if '0' <= c && c <= '9' {
        true
    } else if 'a' <= c && c <= 'z' {
        true
    } else if 'A' <= c && c <= 'Z' {
        true
    } else if c == '_' {
        true
    } else {
        false
    }
}

pub fn is_c_identifier(s: &str) -> bool {
    for c in s.chars() {
        if !is_c_identifier_char(c) {
            return false;
        }
    }
    true
}

//--------------------------------------------------------------------------------------------------
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test() {
        println!("");
        {
            let code = "typedef void // some comment    \n /* some other comment     */ (VKAPI_PTR *PFN_vkInternalAllocationNotification)(\\\r\n    void*                                       pUserData = M_PI/4,\r\n    size_t                                      size,\\\n    VkInternalAllocationType                    allocationType, \n    VkSystemAllocationScope                     allocationScope);";

            let post_phase_1: String = IterPhase1::new(code).collect();
            let post_phase_1_ref = "typedef void // some comment    \n /* some other comment     */ (VKAPI_PTR *PFN_vkInternalAllocationNotification)(\\\n    void*                                       pUserData = M_PI/4,\n    size_t                                      size,\\\n    VkInternalAllocationType                    allocationType, \n    VkSystemAllocationScope                     allocationScope);";
            assert_eq!(&post_phase_1, post_phase_1_ref);

            let post_phase_2: String = IterPhase2::new(code).collect();
            let post_phase_2_ref = "typedef void // some comment    \n /* some other comment     */ (VKAPI_PTR *PFN_vkInternalAllocationNotification)(    void*                                       pUserData = M_PI/4,\n    size_t                                      size,    VkInternalAllocationType                    allocationType, \n    VkSystemAllocationScope                     allocationScope);";
            assert_eq!(&post_phase_2, post_phase_2_ref);

            let post_phase_3: String = IterPhase3a::new(code).collect();
            let post_phase_3_ref = "typedef void \n (VKAPI_PTR *PFN_vkInternalAllocationNotification)( void* pUserData = M_PI/4,\n size_t size, VkInternalAllocationType allocationType, \n VkSystemAllocationScope allocationScope);";
            assert_eq!(&post_phase_3, post_phase_3_ref);
        }

        {
            let code = "// DEPRECATED: This define has been removed. Specific version defines (e.g. VK_API_VERSION_1_0), or the VK_MAKE_VERSION macro, should be used instead.\n//#define VK_API_VERSION VK_MAKE_VERSION(1, 0, 0) // Patch version should always be set to 0";
            let post_phase_3: String = IterPhase3a::new(code).collect();
            assert_eq!(&post_phase_3, " \n ");
        }

        {
            let code = "#define x y\n //some comment here\n class Something;\n";
            let mut token_iter = IterTokenInner::new(code);
            assert_eq!(
                token_iter.next(),
                Some(Token::PpDirective(PpDirective::Define))
            );
            assert_eq!(token_iter.next(), Some(Token::Identifier("x")));
            assert_eq!(token_iter.next(), Some(Token::Identifier("y")));
            assert_eq!(token_iter.next(), Some(Token::NewLine));
            assert_eq!(token_iter.next(), Some(Token::Identifier("class")),);
            assert_eq!(token_iter.next(), Some(Token::Identifier("Something")),);
            assert_eq!(token_iter.next(), Some(Token::Punctuation));
        }
    }
}
