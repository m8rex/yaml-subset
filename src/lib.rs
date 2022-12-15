extern crate pest;
#[macro_use]
extern crate pest_derive;

use linked_hash_map::LinkedHashMap;
use pest::iterators::{Pair, Pairs};
use pest::Parser;

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

use pest::error::Error;

fn parse_yaml_file(file: &str) -> Result<Vec<DocumentData>, Error<Rule>> {
    let pairs = YamlParser::parse(Rule::yaml, file)?
        .next()
        .unwrap()
        .into_inner();
    Ok(pairs
        .filter_map(|p| match p.as_rule() {
            Rule::document => Some(DocumentData::Yaml(parse_value(
                p.into_inner().next().unwrap(),
            ))),
            Rule::commentnl | Rule::comment => Some(DocumentData::Comment(comment(p))),
            Rule::EOI => None,
            _ => unreachable!("{:?}", p),
        })
        .collect())
}

fn comment(pair: Pair<Rule>) -> String {
    pair.into_inner().next().unwrap().as_str().to_string()
}

fn alias(pair: Pair<Rule>) -> String {
    pair.into_inner().next().unwrap().as_str().to_string()
}

fn parse_value(pair: Pair<Rule>) -> Yaml {
    match pair.as_rule() {
        Rule::hash => Yaml::Hash(
            pair.into_inner()
                .map(|pair| parse_hash_data(pair))
                .collect(),
        ),
        Rule::array => Yaml::Array(
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
}
