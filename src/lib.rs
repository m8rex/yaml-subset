extern crate pest;
#[macro_use]
extern crate pest_derive;

pub use crate::path::YamlPath;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::fmt::Write;

mod path {
    use pest_consume::{match_nodes, Error, Parser};
    type PathResult<T> = std::result::Result<T, Error<Rule>>;
    type Node<'i> = pest_consume::Node<'i, Rule, ()>;

    #[derive(Parser)]
    #[grammar = "path.pest"]
    struct PathParser;

    #[pest_consume::parser]
    impl PathParser {
        fn EOI(_input: Node) -> PathResult<()> {
            Ok(())
        }
        fn key(input: Node) -> PathResult<YamlPath> {
            Ok(YamlPath::Key(input.as_str().to_string(), None))
        }
        fn index_number(input: Node) -> PathResult<YamlPath> {
            Ok(YamlPath::Indexes(
                vec![input.as_str().parse().unwrap()],
                None,
            )) // TODO: remove unwrap
        }
        fn index_all(input: Node) -> PathResult<YamlPath> {
            Ok(YamlPath::AllIndexes(None))
        }
        fn part(input: Node) -> PathResult<YamlPath> {
            Ok(match_nodes!(input.into_children();
                [index_all(r)] => r,
                [index_number(r)] => r,
                [key(r)] => r,

            ))
        }
        fn path(input: Node) -> PathResult<YamlPath> {
            Ok(match_nodes!(input.into_children();
                [key(s), part(vals)..] => {
                    let mut path = s;
                    for val in vals.into_iter() {
                        path.insert(val);
                    }
                    path
                }
            ))
        }
        fn final_path(input: Node) -> PathResult<YamlPath> {
            Ok(match_nodes!(input.into_children();
                [path(p), EOI(_)] => p
            ))
        }
    }

    fn parse(input_str: &str) -> PathResult<YamlPath> {
        // Parse the input into `Nodes`
        let inputs = PathParser::parse(Rule::final_path, input_str)?;
        // There should be a single root node in the parsed tree
        let input = inputs.single()?;
        // Consume the `Node` recursively into the final value
        PathParser::final_path(input)
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum YamlPath {
        Key(String, Option<Box<YamlPath>>),
        AllIndexes(Option<Box<YamlPath>>),
        Indexes(Vec<usize>, Option<Box<YamlPath>>),
    }

    impl std::str::FromStr for YamlPath {
        type Err = pest_consume::Error<Rule>;

        fn from_str(path_str: &str) -> PathResult<Self> {
            parse(path_str)
        }
    }

    impl YamlPath {
        pub fn insert(&mut self, p: YamlPath) {
            match self {
                YamlPath::Key(_, ref mut o) => *o = Some(Box::new(p)),
                YamlPath::AllIndexes(ref mut o) => *o = Some(Box::new(p)),
                YamlPath::Indexes(_, ref mut o) => *o = Some(Box::new(p)),
            }
        }
    }
}

#[derive(Parser)]
#[grammar = "yaml.pest"]
struct YamlParser;

#[derive(Debug, Clone, PartialEq)]
pub enum HashData {
    InlineComment(String),
    Comment(String),
    Element(HashElement),
}

impl YamlInsert for HashData {
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize {
        match self {
            HashData::Element(e) => e.insert_into_hash(path, h, overwrite),
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct HashElement {
    pub key: String,
    pub value: AliasedYaml,
}

impl YamlInsert for HashElement {
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize {
        self.value.insert_into_hash(path, h, overwrite)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayData {
    InlineComment(String),
    Comment(String),
    Element(AliasedYaml),
}

impl YamlInsert for ArrayData {
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize {
        match self {
            ArrayData::Element(a) => a.insert_into_hash(path, h, overwrite),
            _ => 0,
        }
    }
}

impl ArrayData {
    pub fn element(e: AliasedYaml) -> ArrayData {
        ArrayData::Element(e)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Yaml {
    EmptyInlineHash,
    Hash(Vec<HashData>),
    InlineArray(Vec<Yaml>),
    Array(Vec<ArrayData>),
    QuotedString(String),
    UnquotedString(String),
    FoldedString(Vec<String>),
    LiteralString(Vec<String>),
    Anchor(String),
}

pub trait YamlInsert {
    /// Insert AliasedYaml into a hash
    /// Returns the amount of insertions.
    /// Can be more than 1 when using indexes or all array elements etc
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize;
}

impl Yaml {
    fn key_index(&self, key: &String) -> Option<usize> {
        match self {
            Yaml::Hash(data) => data
                .iter()
                .enumerate()
                .find(|(_, v)| match v {
                    HashData::Element(e) => &e.key == key,
                    _ => false,
                })
                .map(|(i, _)| i),
            _ => None,
        }
    }
}
impl YamlInsert for Yaml {
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize {
        match path {
            YamlPath::Key(k, other_path_opt) => {
                let key_exists = self.key_index(&k);
                match self {
                    Yaml::Hash(data) => {
                        if let Some(other_path) = other_path_opt {
                            if let Some(index) = key_exists {
                                data[index].insert_into_hash(&*other_path, h, overwrite)
                            } else {
                                0
                            }
                        } else {
                            let value = HashData::Element(HashElement {
                                key: k.to_owned(),
                                value: h.to_owned(),
                            });
                            if key_exists.is_none() {
                                data.push(value);
                                1
                            } else if let Some(index) = key_exists {
                                if overwrite {
                                    data[index] = value;
                                    1
                                } else {
                                    0
                                }
                            } else {
                                0
                            }
                        }
                    }
                    _ => 0,
                }
            }
            YamlPath::Indexes(indexes, Some(other_path)) => match self {
                Yaml::InlineArray(elements) => {
                    let mut count = 0;
                    for index in indexes.iter() {
                        let element_opt = elements.get_mut(*index);
                        if let Some(element) = element_opt {
                            count += element.insert_into_hash(&*other_path, h, overwrite)
                        }
                    }
                    count
                }
                Yaml::Array(elements) => {
                    let mut elements: Vec<_> = elements
                        .iter_mut()
                        .filter(|e| matches!(e, ArrayData::Element(_)))
                        .collect();
                    let mut count = 0;
                    for index in indexes.iter() {
                        let element_opt = elements.get_mut(*index);
                        if let Some(element) = element_opt {
                            count += (*element).insert_into_hash(&*other_path, h, overwrite)
                        }
                    }
                    count
                }
                _ => 0,
            },
            YamlPath::AllIndexes(Some(other_path)) => match self {
                Yaml::InlineArray(elements) => {
                    let mut count = 0;
                    for element in elements.iter_mut() {
                        count += element.insert_into_hash(&*other_path, h, overwrite)
                    }
                    count
                }
                Yaml::Array(elements) => {
                    let mut count = 0;
                    for element in elements.iter_mut() {
                        match element {
                            ArrayData::Element(element) => {
                                count += element.insert_into_hash(&*other_path, h, overwrite)
                            }
                            _ => (),
                        }
                    }
                    count
                }
                _ => 0,
            },
            YamlPath::Indexes(_, None) | YamlPath::AllIndexes(None) => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AliasedYaml {
    pub alias: Option<String>,
    pub value: Yaml,
}

impl YamlInsert for AliasedYaml {
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize {
        self.value.insert_into_hash(path, h, overwrite)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DocumentData {
    Comment(String),
    Yaml(Yaml),
}

impl YamlInsert for DocumentData {
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize {
        match self {
            DocumentData::Yaml(y) => y.insert_into_hash(path, h, overwrite),
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Document {
    items: Vec<DocumentData>,
}

impl YamlInsert for Document {
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize {
        let mut count = 0;
        for item in self.items.iter_mut() {
            count += item.insert_into_hash(path, h, overwrite);
        }
        count
    }
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
            Yaml::EmptyInlineHash => {
                write!(f, " {{}}")
            }
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
            Yaml::InlineArray(v) => {
                write!(f, " [")?;
                for (idx, element) in v.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, " ",)?;
                    element.format(f, spaces, None);
                }
                write!(f, " ]")
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
            Yaml::UnquotedString(s) => {
                write!(f, " {}", s)
            }
            Yaml::QuotedString(s) => {
                write!(f, " \"{}\"", s)
            }
            Yaml::LiteralString(lines) => {
                write!(f, " |")?;
                for line in lines.iter() {
                    write!(f, "\n{}{}", indent(spaces), line)?;
                }
                Ok(())
            }
            Yaml::FoldedString(lines) => {
                write!(f, " >")?;
                for line in lines.iter() {
                    write!(f, "\n{}{}", indent(spaces), line)?;
                }
                Ok(())
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
fn parse_literal(lines: Vec<&str>) -> String {
    let s = lines.join("\n");
    handle_ending_newlines(s)
}

/// Parse folded string to string
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
        Rule::inline_array_string => parse_value(pair.into_inner().next().unwrap()),
        Rule::string => parse_value(pair.into_inner().next().unwrap()),
        Rule::unquoted_string => {
            Yaml::UnquotedString(pair.as_str().to_string().trim_end_matches(" ").to_string())
        }
        Rule::unquoted_inline_string => {
            Yaml::UnquotedString(pair.as_str().to_string().trim_end_matches(" ").to_string())
        }
        Rule::quoted_string => {
            Yaml::QuotedString(pair.into_inner().next().unwrap().as_str().to_string())
        }
        Rule::string_multiline_literal => Yaml::LiteralString(
            pair.into_inner()
                .map(|p| p.as_str().to_string())
                .collect::<Vec<_>>(),
        ),
        Rule::string_multiline_folded => Yaml::FoldedString(
            pair.into_inner()
                .map(|p| p.as_str().to_string())
                .collect::<Vec<_>>(),
        ),
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
    use super::{AliasedYaml, HashData, HashElement, Yaml, YamlInsert, YamlPath};

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
    fn block_delimiters_in_text() {
        let inp = r#"---
content: When you press <em>Submit part</em>.
abs: An absolute value is written as |x|
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

    #[test]
    fn insert_hash_top() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "new".parse().unwrap();
        let data = AliasedYaml {
            alias: None,
            value: Yaml::UnquotedString("value".to_string()),
        };
        assert_eq!(1, parsed.insert_into_hash(&path, &data, false));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
new: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }

    #[test]
    fn insert_hash() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key2".parse().unwrap();
        let data = AliasedYaml {
            alias: None,
            value: Yaml::QuotedString("test".to_string()),
        };
        assert_eq!(1, parsed.insert_into_hash(&path, &data, false));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
 key2: "test"
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);

        let path: YamlPath = "item.key2".parse().unwrap();
        let data = AliasedYaml {
            alias: None,
            value: Yaml::QuotedString("test2".to_string()),
        };
        assert_eq!(0, parsed.insert_into_hash(&path, &data, false));
        assert_eq!(parsed_out, parsed);
        assert_eq!(1, parsed.insert_into_hash(&path, &data, true));
        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
 key2: "test2"
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
}
