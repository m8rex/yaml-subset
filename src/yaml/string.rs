/// handle ending newlines for literal and folded strings
fn handle_ending_newlines(s: String) -> String {
    let ends_with_newline = s.ends_with("\n");
    let mut result = s.trim_end_matches("\n").to_string();
    if ends_with_newline {
        result.push_str("\n");
    }
    result
}

/// Parse literal string to string
pub fn parse_literal(lines: Vec<&str>) -> String {
    let s = lines.join("\n");
    handle_ending_newlines(s)
}

/// Parse folded string to string
pub fn parse_folded(lines: Vec<&str>) -> String {
    if lines.is_empty() {
        return String::new();
    }

    let start = lines[0].to_string();

    let result = lines
        .into_iter()
        .skip(1)
        .fold((start, false), |(s, prev_was_empty), element| {
            let mut c = s.clone(); // TODO
            let is_empty = if element.is_empty() {
                c.push_str("\n");
                true
            } else if element.starts_with(" ") {
                c.push_str("\n");
                c.push_str(element);
                c.push_str("\n");
                true
            } else {
                if !prev_was_empty {
                    c.push_str(" ");
                }
                c.push_str(element);
                false
            };
            (c, is_empty)
        })
        .0;
    handle_ending_newlines(result)
}

#[derive(Debug, Clone, PartialEq)]
pub enum SingleQuotedStringPart {
    String(String),
    EscapedChar(SingleQuotedStringEscapedChar),
    BlankLines(usize),
    RemovableNewline,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SingleQuotedStringEscapedChar {
    SingleQuote,
}

impl SingleQuotedStringEscapedChar {
    pub fn char(&self) -> char {
        match self {
            SingleQuotedStringEscapedChar::SingleQuote => '\'',
        }
    }
}

pub fn parse_single_quoted_string(parts: Vec<SingleQuotedStringPart>) -> String {
    let mut result = String::new();
    for part in parts {
        match part {
            SingleQuotedStringPart::String(s) => result.push_str(&s),
            SingleQuotedStringPart::EscapedChar(c) => result.push(c.char()),
            SingleQuotedStringPart::BlankLines(nb) => {
                for _ in 0..nb {
                    result.push_str("\n");
                }
            }
            SingleQuotedStringPart::RemovableNewline => (),
        }
    }
    result
}

#[derive(Debug, Clone, PartialEq)]
pub enum DoubleQuotedStringPart {
    String(String),
    EscapedChar(DoubleQuotedStringEscapedChar),
    BlankLines(usize),
    RemovableNewline,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum DoubleQuotedStringEscapedChar {
    Quote,
    Backslash,
    Tab,
    CarriageReturn,
    Newline,
    RealNewline,
}

impl DoubleQuotedStringEscapedChar {
    pub fn char(&self) -> char {
        match self {
            DoubleQuotedStringEscapedChar::Quote => '"',
            DoubleQuotedStringEscapedChar::Backslash => '\\',
            DoubleQuotedStringEscapedChar::Tab => 't',
            DoubleQuotedStringEscapedChar::CarriageReturn => 'r',
            DoubleQuotedStringEscapedChar::Newline => 'n',
            DoubleQuotedStringEscapedChar::RealNewline => '\n',
        }
    }
    pub fn real_char(&self) -> char {
        match self {
            DoubleQuotedStringEscapedChar::Quote => '"',
            DoubleQuotedStringEscapedChar::Backslash => '\\',
            DoubleQuotedStringEscapedChar::Tab => '\t',
            DoubleQuotedStringEscapedChar::CarriageReturn => '\r',
            DoubleQuotedStringEscapedChar::Newline => '\n',
            DoubleQuotedStringEscapedChar::RealNewline => '\n',
        }
    }
}

pub fn parse_double_quoted_string(parts: Vec<DoubleQuotedStringPart>) -> String {
    let mut s = String::new();
    for part in parts {
        match part {
            DoubleQuotedStringPart::String(s2) => s.push_str(&s2),
            DoubleQuotedStringPart::EscapedChar(c) => s.push(c.real_char()),
            DoubleQuotedStringPart::BlankLines(n) => {
                for _ in 0..n {
                    s.push_str("\n");
                }
            }
            DoubleQuotedStringPart::RemovableNewline => (),
        }
    }
    s
}

#[cfg(test)]
mod tests {
    #[test]
    fn parse_folded() {
        let input = vec![
            "Several lines of text,",
            r#"with some "quotes" of various 'types',"#,
            "and also a blank line:",
            "",
            "and some text with",
            "  extra indentation",
            "on the next line,",
            "plus another line at the end.",
            "",
            "",
        ];
        assert_eq!(
            super::parse_folded(input),
            r#"Several lines of text, with some "quotes" of various 'types', and also a blank line:
and some text with
  extra indentation
on the next line, plus another line at the end.
"#
        )
    }

    #[test]
    fn parse_literal() {
        let input = vec![
            "Several lines of text,",
            r#"with some "quotes" of various 'types',"#,
            "and also a blank line:",
            "",
            "and some text with",
            "  extra indentation",
            "on the next line,",
            "plus another line at the end.",
            "",
            "",
        ];
        assert_eq!(
            super::parse_literal(input),
            r#"Several lines of text,
with some "quotes" of various 'types',
and also a blank line:

and some text with
  extra indentation
on the next line,
plus another line at the end.
"#
        )
    }
}
