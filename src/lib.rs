extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::fmt::Write;

#[derive(Parser)]
#[grammar = "yaml.pest"]
struct YamlParser;

#[derive(Debug, Clone, PartialEq)]
pub enum HashData {
    InlineComment(String),
    Comment(String),
    Element(HashElement),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HashElement {
    key: String,
    value: AliasedYaml,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayData {
    InlineComment(String),
    Comment(String),
    Element(AliasedYaml),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Yaml {
    Hash(Vec<HashData>),
    Array(Vec<ArrayData>),
    String(String),
    Anchor(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct AliasedYaml {
    alias: Option<String>,
    value: Yaml,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DocumentData {
    Comment(String),
    Yaml(Yaml),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Document {
    items: Vec<DocumentData>,
}

fn indent(amount: usize) -> String {
    (0..amount).map(|_| " ").collect::<Vec<_>>().join("")
}

impl Document {
    pub fn format(&self) -> Result<String, std::fmt::Error> {
        let mut s = String::new();
        write!(s, "---")?;
        for item in self.items.iter() {
            match item {
                DocumentData::Comment(c) => write!(&mut s, "\n#{}", c),
                DocumentData::Yaml(y) => y.format(&mut s, 0, None),
            }?;
        }
        Ok(s)
    }
}

#[derive(Debug, Clone, Copy)]
enum Parent {
    Hash,
    Array,
}

impl AliasedYaml {
    fn format(&self, f: &mut String, spaces: usize, parent: Option<Parent>) -> std::fmt::Result {
        if let Some(alias) = self.alias.as_ref() {
            write!(f, " &{}", alias)?;
            self.value.format(f, spaces + 2, None)
        } else {
            self.value.format(f, spaces + 2, parent)
        }
    }
}

fn should_write_literal(s: &String) -> bool {
    s.contains("\n") || s.contains("\\") // TODO
}

fn needs_quotes(s: &String) -> bool {
    s.is_empty() || s.contains(": ") || s.contains(" #")
}

impl Yaml {
    fn format(&self, f: &mut String, spaces: usize, parent: Option<Parent>) -> std::fmt::Result {
        match self {
            Yaml::Hash(v) => {
                if v.is_empty() {
                    return write!(f, " {{}}");
                }
                for (idx, element) in v.iter().enumerate() {
                    match element {
                        HashData::Comment(c) => {
                            write!(f, "\n{}#{}", indent(spaces), c)
                        }
                        HashData::InlineComment(c) => write!(f, " #{}", c),
                        HashData::Element(v) => {
                            let element = Some(Parent::Hash);
                            let same_line = match (element, parent) {
                                (Some(_), Some(Parent::Array)) => idx == 0,
                                _ => false,
                            };

                            if same_line {
                                write!(f, " {}:", v.key)?;
                            } else {
                                writeln!(f, "")?;
                                write!(f, "{}{}:", indent(spaces), v.key)?;
                            };

                            v.value.format(f, spaces, element)
                        }
                    }?;
                }
                Ok(())
            }
            Yaml::Array(v) => {
                if v.is_empty() {
                    return write!(f, " []");
                }
                for (idx, element) in v.iter().enumerate() {
                    match element {
                        ArrayData::Comment(c) => write!(f, "\n{}#{}", indent(spaces), c),
                        ArrayData::InlineComment(c) => write!(f, " #{}", c),
                        ArrayData::Element(v) => {
                            let element = Some(Parent::Array);
                            let same_line = match (element, parent) {
                                (Some(_), Some(Parent::Array)) => idx == 0,
                                _ => false,
                            };

                            if same_line {
                                write!(f, " -")?;
                            } else {
                                writeln!(f, "")?;
                                write!(f, "{}-", indent(spaces))?;
                            };

                            v.format(f, spaces, element)
                        }
                    }?
                }
                Ok(())
            }
            Yaml::String(s) => {
                // TODO: Some way to choose between folded or literal?
                if should_write_literal(s) {
                    write!(f, " |")?;
                    for line in s.split("\n") {
                        write!(f, "\n{}{}", indent(spaces), line)?;
                    }
                    Ok(())
                } else {
                    if needs_quotes(s) {
                        write!(f, " \"{}\"", s)
                    } else {
                        write!(f, " {}", s)
                    }
                }
            }
            Yaml::Anchor(a) => write!(f, " *{}", a),
        }
    }
}

use pest::error::Error;

pub fn parse_yaml_file(file: &str) -> Result<Document, Error<Rule>> {
    let pairs = YamlParser::parse(Rule::yaml, file)?
        .next()
        .unwrap()
        .into_inner();
    Ok(Document {
        items: pairs
            .filter_map(|p| match p.as_rule() {
                Rule::document => Some(DocumentData::Yaml(parse_value(
                    p.into_inner().next().unwrap(),
                ))),
                Rule::commentnl | Rule::comment => Some(DocumentData::Comment(comment(p))),
                Rule::EOI => None,
                _ => unreachable!("{:?}", p),
            })
            .collect(),
    })
}

fn comment(pair: Pair<Rule>) -> String {
    pair.into_inner().next().unwrap().as_str().to_string()
}

fn alias(pair: Pair<Rule>) -> String {
    pair.into_inner().next().unwrap().as_str().to_string()
}

fn handle_ending_newlines(s: String) -> String {
    let ends_with_newline = s.ends_with("\n");
    let mut result = s.trim_end_matches("\n").to_string();
    if ends_with_newline {
        result.push_str("\n");
    }
    result
}

fn parse_literal(lines: Vec<&str>) -> String {
    let s = lines.join("\n");
    handle_ending_newlines(s)
}

fn parse_folded(lines: Vec<&str>) -> String {
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

fn parse_value(pair: Pair<Rule>) -> Yaml {
    match pair.as_rule() {
        Rule::hash | Rule::alternative_hash => Yaml::Hash(
            pair.into_inner()
                .map(|pair| parse_hash_data(pair))
                .collect(),
        ),
        Rule::inline_hash => Yaml::Hash(
            // TODO
            Vec::new(),
        ),
        Rule::array | Rule::alternative_array => Yaml::Array(
            pair.into_inner()
                .map(|pair| parse_array_data(pair))
                .collect(),
        ),
        Rule::inline_array_string => {
            Yaml::String(pair.into_inner().next().unwrap().as_str().to_string())
        }
        Rule::string => Yaml::String(pair.into_inner().next().unwrap().as_str().to_string()),
        Rule::string_multiline_literal => Yaml::String(parse_literal(
            pair.into_inner().map(|p| p.as_str()).collect::<Vec<_>>(),
        )),
        Rule::string_multiline_folded => Yaml::String(parse_folded(
            pair.into_inner().map(|p| p.as_str()).collect::<Vec<_>>(),
        )),
        Rule::anchor => Yaml::Anchor(pair.into_inner().next().unwrap().as_str().to_string()),
        _ => unreachable!("{:?}", pair),
    }
}

fn parse_hash_data(pair: Pair<Rule>) -> HashData {
    match pair.as_rule() {
        Rule::hash_element => HashData::Element(parse_hash_element(pair.into_inner())),
        Rule::commentnl => HashData::Comment(comment(pair)),
        Rule::comment => HashData::InlineComment(comment(pair)),
        _ => unreachable!(),
    }
}

fn parse_hash_element(mut pairs: Pairs<Rule>) -> HashElement {
    let key = pairs.next().unwrap().as_str().to_string();
    let item2 = pairs.next().unwrap();
    return HashElement {
        key,
        value: if let Some(value) = pairs.next() {
            AliasedYaml {
                alias: Some(alias(item2)),
                value: parse_value(value),
            }
        } else {
            AliasedYaml {
                alias: None,
                value: parse_value(item2),
            }
        },
    };
}

fn parse_array_data(pair: Pair<Rule>) -> ArrayData {
    match pair.as_rule() {
        Rule::commentnl => ArrayData::Comment(comment(pair)),
        Rule::comment => ArrayData::InlineComment(comment(pair)),
        Rule::block_array_element => ArrayData::Element(parse_array_element(pair.into_inner())),
        _ => ArrayData::Element(AliasedYaml {
            alias: None,
            value: parse_value(pair),
        }),
    }
}

fn parse_array_element(mut pairs: Pairs<Rule>) -> AliasedYaml {
    let item1 = pairs.next().unwrap();

    if let Some(value) = pairs.next() {
        AliasedYaml {
            alias: Some(alias(item1)),
            value: parse_value(value),
        }
    } else {
        AliasedYaml {
            alias: None,
            value: parse_value(item1),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::parse_yaml_file;

    #[test]
    fn indent() {
        assert_eq!(super::indent(1), " ".to_string());
        assert_eq!(super::indent(2), "  ".to_string());
        assert_eq!(super::indent(3), "   ".to_string());
        assert_eq!(super::indent(4), "    ".to_string());
        assert_eq!(super::indent(5), "     ".to_string());
        assert_eq!(super::indent(6), "      ".to_string());
        assert_eq!(super::indent(7), "       ".to_string());
        assert_eq!(super::indent(8), "        ".to_string());
    }

    #[test]
    fn basic() {
        let inp = r#"---
# test
k: &anch test
# hi
v: &ok
   - &ok2 test
# comment
   - *test
v: test # maybe
k: [5, 10, "e"]
# after
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn inline_hash() {
        let inp = r#"---
field: &test {}
item: {}
other: # check
  item: {}
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn elab() {
        let inp = r#"---
# test
k: &anch test
# hi
v: &ok
   - &ok2 test
# comment
   - *test # an anchor
v: test # maybe
k: &kk [5, 10, "e"]
# after
y: - x: 5
     z: &zfield 6 # yess
   - n: 8
   - j
# between
z: - *kk
   - ke
k: - - j
     - k
     - l
   - - m
     - z: k
       t: - m: l # comment
            p: 4 # comment
          - m # after m
# wow
# something else
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

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

    #[test]
    fn block_scalars() {
        // see https://yaml-multiline.info/
        let inp = r#"---
        newlines: &newline |
                  Several lines of text,
                  with some "quotes" of various 'types',
                  and also a blank line:
                  
                  and some text with
                    extra indentation
                  on the next line,
                  plus another line at the end.
                  
        folded: &folded >
                  Several lines of text,
                  with some "quotes" of various 'types',
                  and also a blank line:
                  
                  and some text with
                  extra indentation
                  on the next line,
                  plus another line at the end.
                  
                  
                  
        test: 5"#;

        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn commas_in_unquoted_string() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }
}
