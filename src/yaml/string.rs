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

// Create literal string for string
pub fn create_literal(s: String) -> Vec<String> {
    let lines = s.trim_start_matches(" ").split("\n");
    lines.map(|x| x.to_string()).collect()
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

fn split_on_length(input: String, length: usize) -> Vec<String> {
    let mut result = Vec::new();
    let mut s = String::new();
    let mut first = true;
    for word in input.split(" ") {
        if s.len() + word.len() + 1 > length {
            result.push(s.clone());
            s = word.to_string();
        } else {
            if !first {
                s.push_str(" ");
            }
            s.push_str(word);
            first = false;
        }
    }
    if !s.is_empty() {
        result.push(s.clone());
    }
    result
}

// Create folded string for string
pub fn create_folded(s: String, line_length: usize) -> Vec<String> {
    let lines = s.split("\n"); // TODO
    lines
        .flat_map(|x| {
            split_on_length(x.to_string(), line_length)
                .into_iter()
                .chain(vec!["".to_string()].into_iter())
        })
        .collect()
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

pub fn parse_single_quoted_string(parts: &Vec<SingleQuotedStringPart>) -> String {
    let mut result = String::new();
    for part in parts.iter() {
        match part {
            SingleQuotedStringPart::String(s) => result.push_str(&s),
            SingleQuotedStringPart::EscapedChar(c) => result.push(c.char()),
            SingleQuotedStringPart::BlankLines(nb) => {
                for _ in 0..*nb {
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

pub fn parse_double_quoted_string(parts: &Vec<DoubleQuotedStringPart>) -> String {
    let mut s = String::new();
    for part in parts {
        match part {
            DoubleQuotedStringPart::String(s2) => s.push_str(&s2),
            DoubleQuotedStringPart::EscapedChar(c) => s.push(c.real_char()),
            DoubleQuotedStringPart::BlankLines(n) => {
                for _ in 0..*n {
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
    fn create_folded() {
        let input = r#"Several lines of text, with some "quotes" of various 'types', and also a blank line:
and some text with
  extra indentation
on the next line, plus another line at the end.
"#;
        assert_eq!(
            super::create_folded(input.to_string(), 40),
            vec![
                r#"Several lines of text, with some"#,
                r#""quotes" of various 'types', and also a"#,
                "blank line:",
                "",
                "and some text with",
                "",
                "  extra indentation",
                "",
                "on the next line, plus another line at",
                "the end.",
                "",
                "",
            ]
        )
    }

    #[test]
    fn parse_literal() {
        let mut input = vec![
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
        let result = r#"Several lines of text,
with some "quotes" of various 'types',
and also a blank line:

and some text with
  extra indentation
on the next line,
plus another line at the end.
"#;
        assert_eq!(super::parse_literal(input.clone()), result);
        let result = format!(" {}", result); // add leading space as test
        input.pop(); // Remove extra one
        assert_eq!(input, super::create_literal(result))
    }
}
