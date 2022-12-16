extern crate pest;
#[macro_use]
extern crate pest_derive;

use linked_hash_map::LinkedHashMap;
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

fn indent(depth: usize) -> String {
    (0..2 * depth).map(|_| " ").collect::<Vec<_>>().join("")
}

impl Document {
    fn format_depth(&self, f: &mut String, depth: usize) -> std::fmt::Result {
        write!(f, "---")?;
        for item in self.items.iter() {
            match item {
                DocumentData::Comment(c) => write!(f, "\n#{}", c),
                DocumentData::Yaml(y) => y.format(f, depth),
            }?;
        }
        Ok(())
    }
    pub fn format(&self) -> String {
        let mut s = String::new();
        self.format_depth(&mut s, 0).unwrap(); // TODO
        s
    }
}

impl AliasedYaml {
    fn format(&self, f: &mut String, depth: usize) -> std::fmt::Result {
        if let Some(alias) = self.alias.as_ref() {
            write!(f, " &{}", alias)?;
            self.value.format(f, depth + 1)
        } else {
            self.value.format(f, depth + 1)
        }
    }
}

impl Yaml {
    fn format(&self, f: &mut String, depth: usize) -> std::fmt::Result {
        match self {
            Yaml::Hash(v) => {
                for element in v.iter() {
                    match element {
                        HashData::Comment(c) => {
                            writeln!(f, "")?;
                            write!(f, "{}#{}", indent(depth), c)
                        }
                        HashData::InlineComment(c) => write!(f, " #{}", c),
                        HashData::Element(v) => {
                            writeln!(f, "")?;
                            write!(f, "{}{}:", indent(depth), v.key)?;
                            v.value.format(f, depth)
                        }
                    }?;
                }
                Ok(())
            }
            Yaml::Array(v) => {
                for element in v.iter() {
                    match element {
                        ArrayData::Comment(c) => write!(f, "\n{}#{}", indent(depth), c),
                        ArrayData::InlineComment(c) => write!(f, " #{}", c),
                        ArrayData::Element(v) => {
                            writeln!(f, "")?;
                            write!(f, "{}-", indent(depth))?;
                            v.format(f, depth)
                        }
                    }?
                }
                Ok(())
            }
            Yaml::String(s) => {
                write!(f, " {}", s) // TODO
            }
            Yaml::Anchor(a) => write!(f, " *{}", a),
        }
    }
}

use pest::error::Error;

fn parse_yaml_file(file: &str) -> Result<Document, Error<Rule>> {
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

fn parse_value(pair: Pair<Rule>) -> Yaml {
    match pair.as_rule() {
        Rule::hash | Rule::alternative_hash => Yaml::Hash(
            pair.into_inner()
                .map(|pair| parse_hash_data(pair))
                .collect(),
        ),
        Rule::array | Rule::alternative_array => Yaml::Array(
            pair.into_inner()
                .map(|pair| parse_array_data(pair))
                .collect(),
        ),
        Rule::string => Yaml::String(pair.into_inner().next().unwrap().as_str().to_string()),
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
        assert_eq!(super::indent(1), "  ".to_string());
        assert_eq!(super::indent(2), "    ".to_string());
        assert_eq!(super::indent(3), "      ".to_string());
        assert_eq!(super::indent(4), "        ".to_string());
        assert_eq!(super::indent(5), "          ".to_string());
        assert_eq!(super::indent(6), "            ".to_string());
        assert_eq!(super::indent(7), "              ".to_string());
        assert_eq!(super::indent(8), "                ".to_string());
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
        insta::assert_display_snapshot!(parsed.unwrap().format());
    }
}
