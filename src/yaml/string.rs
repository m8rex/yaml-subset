use super::BlockChomping;

/// handle ending newlines for literal and folded strings
fn handle_ending_newlines(s: String, chomping: BlockChomping) -> String {
    match chomping {
        BlockChomping::Keep => {
            let mut result = s;
            result.push('\n');
            result
        }
        BlockChomping::Clip => {
            let mut result = s.trim_end_matches('\n').to_string();
            result.push('\n');
            result
        }
        BlockChomping::Strip => s.trim_end_matches('\n').to_string(),
    }
}

/// Parse literal string to string
pub fn parse_literal(lines: Vec<&str>, chomping: BlockChomping) -> String {
    let s = lines.join("\n");
    handle_ending_newlines(s, chomping)
}

// Create literal string for string.
// Strips the trailing "" that split("\n") produces for strings ending with \n,
// because Keep chomping adds that \n back during deserialization.
pub fn create_literal(s: String) -> Vec<String> {
    let mut lines: Vec<String> = s.split('\n').map(str::to_string).collect();
    if s.ends_with('\n') {
        if lines.last().map(|l| l.is_empty()).unwrap_or(false) {
            lines.pop();
        }
    }
    lines
}

/// Parse folded string to string
pub fn parse_folded(lines: Vec<&str>, chomping: BlockChomping) -> String {
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
            } else {
                let without_indent = &element; // TODO: remove

                if without_indent.starts_with(" ") {
                    c.push_str("\n");
                    c.push_str(without_indent);
                    c.push_str("\n");
                    true
                } else {
                    if !prev_was_empty {
                        c.push_str(" ");
                    }
                    c.push_str(without_indent);
                    false
                }
            };
            (c, is_empty)
        })
        .0;
    handle_ending_newlines(result, chomping)
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

// Create folded string for string.
// Strips two trailing ""s that the chain([""])  pattern adds for strings ending
// with \n: one for the paragraph-break after the last wrapped line, and one for
// the empty split result of the trailing \n.  Keep chomping adds the \n back.
pub fn create_folded(s: String, line_length: usize) -> Vec<String> {
    let ends_with_newline = s.ends_with('\n');
    let mut lines: Vec<String> = s
        .split('\n')
        .flat_map(|x| {
            split_on_length(x.to_string(), line_length)
                .into_iter()
                .chain(std::iter::once(String::new()))
        })
        .collect();
    if ends_with_newline {
        if lines.last().map(|l| l.is_empty()).unwrap_or(false) { lines.pop(); }
        if lines.last().map(|l| l.is_empty()).unwrap_or(false) { lines.pop(); }
    }
    lines
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
    use crate::yaml::BlockChomping;

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
            super::parse_folded(input, BlockChomping::Clip),
            r#"Several lines of text, with some "quotes" of various 'types', and also a blank line:
and some text with
  extra indentation
on the next line, plus another line at the end.
"#
        )
    }
    /* TODO: remove
        #[test]
        fn parse_folded_indent() {
            let input = vec![
                "    Several lines of text,",
                r#"  with some "quotes" of various 'types',"#,
                "  and also a blank line:",
                "",
                "  and some text with",
                "    extra indentation",
                "  on the next line,",
                "  plus another line at the end.",
                "",
                "",
            ];
            assert_eq!(
                super::parse_folded(input),
                r#"  Several lines of text, with some "quotes" of various 'types', and also a blank line:
    and some text with
      extra indentation
    on the next line, plus another line at the end.
    "#
            )
        }*/

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
        assert_eq!(
            super::parse_literal(input.clone(), BlockChomping::default()),
            result
        );
        // Round-trip: create_literal of the parsed string gives back the original lines
        // (minus the two trailing empty strings that represent chomping).
        input.pop();
        input.pop();
        assert_eq!(input, super::create_literal(result.to_string()));

        // Leading spaces in the first line must be preserved (needed for |2 indicator).
        let with_leading = format!(" {}", result);
        let lines_with_leading = super::create_literal(with_leading);
        assert!(lines_with_leading[0].starts_with(' '), "leading space must be preserved")
    }

    /* TODO: remove
        #[test]
        fn parse_literal_indent() {
            let mut input = vec![
                "    Several lines of text,",
                r#"  with some "quotes" of various 'types',"#,
                "  and also a blank line:",
                "",
                "  and some text with",
                "    extra indentation",
                "  on the next line,",
                "  plus another line at the end.",
                "",
                "",
            ];
            let result = r#"  Several lines of text,
    with some "quotes" of various 'types',
    and also a blank line:

    and some text with
      extra indentation
    on the next line,
    plus another line at the end.
    "#;
            assert_eq!(super::parse_literal(&Some(2), input.clone()), result);
            let result = format!(" {}", result); // add leading space as test
            input.pop(); // Remove extra one
            assert_eq!(input, super::create_literal(result))
        }*/
}
