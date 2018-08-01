#![allow(dead_code)]

//--------------------------------------------------------------------------------------------------
/* Performs Phase 1 of C compilation, specifically replaces platform-specific end-of-line indicators
 with newlines characters and transforms trigraph sequences.*/
pub struct IterPhase1<'a> {
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
pub struct IterPhase2<'a> {
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
/** Part of phase 3 that deals with comments and whitespace. Does not produce tokens, just filters
and transforms characters. */
pub struct IterPhase3a<'a> {
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
enum Token {
    PpDirective(PpDirective),
    Punctuation,
    HeaderName(String),
    Identifier(String),
    StringLiteral(String),
    CharacterLiteral(char),
    NewLine,
}

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

enum LineState {
    Start,
    PpDirective,
    PpDirectiveBody,
    Normal,
}

struct IterToken<'a> {
    src: IterPhase3a<'a>,
    buf: String,
    peek: Option<char>,
    line: LineState,
}

impl<'a> IterToken<'a> {
    fn new(code: &'a str) -> Self {
        let mut src = IterPhase3a::new(code);
        let peek = src.next();
        Self {
            src,
            buf: String::new(),
            peek,
            line: LineState::Start,
        }
    }
}
/*
impl<'a> Iterator for IterToken<'a> {
    type Item = PpToken;
    fn next(&mut self) -> Option<PpToken> {
        loop {
            let c = if let Some(c) = self.peek {
                c
            } else {
                return None;
            };
            self.peek = self.src.next();
            match c {
                ' ' => (),
                '\n' => {
                    let line = self.line;
                    self.line = Line::Start;
                    if let LineState::PpLine = line {
                        return Some(PpToken::NewLine);
                    }
                }
                '#' => match self.line {
                    LineState::Pp => expr,
                    None => expr,
                },
                _ => panic!("{:?}", c),
            }
        }
    }
}*/

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

#[allow(dead_code)]
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
    }

}
