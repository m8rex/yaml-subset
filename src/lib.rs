extern crate pest;
#[macro_use]
extern crate pest_derive;

pub use crate::path::YamlPath;
use path::Condition;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use std::fmt::Write;
use std::ops::Fn;

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
        fn pure_key(input: Node) -> PathResult<String> {
            Ok(input.as_str().to_string())
        }
        fn field(input: Node) -> PathResult<String> {
            Ok(input.as_str().to_string())
        }
        fn value(input: Node) -> PathResult<String> {
            Ok(input.as_str().to_string())
        }
        fn condition(input: Node) -> PathResult<Condition> {
            Ok(match_nodes!(input.into_children();
                [field(f), value(v)] => Condition { field: f, value: v }
            ))
        }
        fn key(input: Node) -> PathResult<YamlPath> {
            Ok(match_nodes!(input.into_children();
                [pure_key(key), condition(cs)..] => YamlPath::Key(key, cs.collect(), None)
            ))
        }

        fn index_number(input: Node) -> PathResult<Vec<usize>> {
            Ok(vec![input.as_str().parse().unwrap()]) // TODO: remove unwrap
        }
        fn index_all(input: Node) -> PathResult<()> {
            Ok(())
        }
        fn part(input: Node) -> PathResult<YamlPath> {
            Ok(match_nodes!(input.into_children();
                [index_all(_), condition(cs)..] => YamlPath::AllIndexes(cs.collect(), None),
                [index_number(indexes), condition(cs)..] => YamlPath::Indexes(indexes, cs.collect(), None),
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
        fn root_key(_input: Node) -> PathResult<()> {
            Ok(())
        }
        fn root(input: Node) -> PathResult<YamlPath> {
            Ok(match_nodes!(input.into_children();
                [root_key(_s), condition(cs)..] => {
                YamlPath::Root(cs.collect())
                }
            ))
        }
        fn final_path(input: Node) -> PathResult<YamlPath> {
            Ok(match_nodes!(input.into_children();
                [path(p), EOI(_)] => p,
                [root(p), EOI(_)] => p
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
    pub struct Condition {
        pub field: String,
        pub value: String,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum YamlPath {
        Root(Vec<Condition>),
        Key(String, Vec<Condition>, Option<Box<YamlPath>>),
        AllIndexes(Vec<Condition>, Option<Box<YamlPath>>),
        Indexes(Vec<usize>, Vec<Condition>, Option<Box<YamlPath>>),
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
                YamlPath::Root(_) => *self = p,
                YamlPath::Key(_, _, Some(ref mut o)) => o.insert(p),
                YamlPath::AllIndexes(_, Some(ref mut o)) => o.insert(p),
                YamlPath::Indexes(_, _, Some(ref mut o)) => o.insert(p),
                YamlPath::Key(_, _, ref mut o) => *o = Some(Box::new(p)),
                YamlPath::AllIndexes(_, ref mut o) => *o = Some(Box::new(p)),
                YamlPath::Indexes(_, _, ref mut o) => *o = Some(Box::new(p)),
            }
        }
    }
    #[cfg(test)]
    mod test {
        use super::*;
        #[test]
        fn parse() {
            assert_eq!("".parse(), Ok(YamlPath::Root(Vec::new())));
            assert_eq!(
                "item".parse(),
                Ok(YamlPath::Key("item".to_string(), Vec::new(), None))
            );
            assert_eq!(
                "item.item2".parse(),
                Ok(YamlPath::Key(
                    "item".to_string(),
                    Vec::new(),
                    Some(Box::new(YamlPath::Key(
                        "item2".to_string(),
                        Vec::new(),
                        None
                    )))
                ))
            );
            assert_eq!(
                "item.item2.item3".parse(),
                Ok(YamlPath::Key(
                    "item".to_string(),
                    Vec::new(),
                    Some(Box::new(YamlPath::Key(
                        "item2".to_string(),
                        Vec::new(),
                        Some(Box::new(YamlPath::Key(
                            "item3".to_string(),
                            Vec::new(),
                            None
                        )))
                    )))
                ))
            );
        }

        #[test]
        fn parse_with_conditions() {
            assert_eq!(
                r#"item|field=test.item2"#.parse(),
                Ok(YamlPath::Key(
                    "item".to_string(),
                    vec![Condition {
                        field: "field".to_string(),
                        value: "test".to_string()
                    }],
                    Some(Box::new(YamlPath::Key(
                        "item2".to_string(),
                        Vec::new(),
                        None
                    )))
                ))
            );
            assert_eq!(
                r#"item|field=test|other=something.item2|yeah=oh"#.parse(),
                Ok(YamlPath::Key(
                    "item".to_string(),
                    vec![
                        Condition {
                            field: "field".to_string(),
                            value: "test".to_string()
                        },
                        Condition {
                            field: "other".to_string(),
                            value: "something".to_string()
                        }
                    ],
                    Some(Box::new(YamlPath::Key(
                        "item2".to_string(),
                        vec![Condition {
                            field: "yeah".to_string(),
                            value: "oh".to_string()
                        }],
                        None
                    )))
                ))
            );
            assert_eq!(
                r#"item[*]|field=test|other=something.item2|yeah=oh"#.parse(),
                Ok(YamlPath::Key(
                    "item".to_string(),
                    Vec::new(),
                    Some(Box::new(YamlPath::AllIndexes(
                        vec![
                            Condition {
                                field: "field".to_string(),
                                value: "test".to_string()
                            },
                            Condition {
                                field: "other".to_string(),
                                value: "something".to_string()
                            }
                        ],
                        Some(Box::new(YamlPath::Key(
                            "item2".to_string(),
                            vec![Condition {
                                field: "yeah".to_string(),
                                value: "oh".to_string()
                            }],
                            None
                        )))
                    )))
                ))
            );
        }

        #[test]
        fn insert() {
            let first: YamlPath = "minimum_marks".parse().unwrap();
            let mut new: YamlPath = "parts[*].gaps|type=gapfill[*]".parse().unwrap();
            new.insert(first);
            assert_eq!(
                new,
                "parts[*].gaps|type=gapfill[*].minimum_marks"
                    .parse()
                    .unwrap()
            );

            let first: YamlPath = "minimum_marks".parse().unwrap();
            let mut new: YamlPath = "parts[*]|type=gapfill.gaps[*]".parse().unwrap();
            new.insert(first);
            assert_eq!(
                new,
                "parts[*]|type=gapfill.gaps[*].minimum_marks"
                    .parse()
                    .unwrap()
            );
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
    fn edit_hash_structure<F>(&mut self, path: &YamlPath, f: &F) -> usize
    where
        F: Fn(&mut Vec<HashData>, String, Option<usize>) -> usize,
    {
        match self {
            HashData::Element(e) => e.edit_hash_structure(path, f),
            _ => 0,
        }
    }
    fn for_hash<F, R>(&mut self, path: &YamlPath, f: &F, r: &R) -> usize
    where
        F: Fn(&mut HashElement) -> usize,
        R: Fn(&mut Yaml) -> usize,
    {
        match self {
            HashData::Element(e) => e.for_hash(path, f, r),
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
    fn edit_hash_structure<F>(&mut self, path: &YamlPath, f: &F) -> usize
    where
        F: Fn(&mut Vec<HashData>, String, Option<usize>) -> usize,
    {
        self.value.edit_hash_structure(path, f)
    }
    fn for_hash<F, R>(&mut self, path: &YamlPath, f: &F, r: &R) -> usize
    where
        F: Fn(&mut HashElement) -> usize,
        R: Fn(&mut Yaml) -> usize,
    {
        match path {
            YamlPath::Root(_conditions) => f(self),
            YamlPath::Key(key, _conditions, None) => {
                if key == &self.key {
                    f(self)
                } else {
                    0
                }
            }
            YamlPath::Key(_key, _conditions, Some(other)) => self.value.for_hash(&*other, f, r),
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ArrayData {
    InlineComment(String),
    Comment(String),
    Element(AliasedYaml),
}

impl YamlInsert for ArrayData {
    fn edit_hash_structure<F>(&mut self, path: &YamlPath, f: &F) -> usize
    where
        F: Fn(&mut Vec<HashData>, String, Option<usize>) -> usize,
    {
        match self {
            ArrayData::Element(a) => a.edit_hash_structure(path, f),
            _ => 0,
        }
    }
    fn for_hash<F, R>(&mut self, path: &YamlPath, f: &F, r: &R) -> usize
    where
        F: Fn(&mut HashElement) -> usize,
        R: Fn(&mut Yaml) -> usize,
    {
        match self {
            ArrayData::Element(a) => match path {
                YamlPath::Root(_conditions) => 0,
                YamlPath::Key(_key, _conditions, None) => a.for_hash(path, f, r),
                YamlPath::AllIndexes(_, Some(other)) => a.for_hash(&*other, f, r),
                YamlPath::AllIndexes(_, None) => a.for_hash(&YamlPath::Root(Vec::new()), f, r),
                YamlPath::Indexes(_, _, Some(other)) => a.for_hash(&*other, f, r),
                YamlPath::Indexes(_, _, None) => a.for_hash(&YamlPath::Root(Vec::new()), f, r),
                _ => 0,
            },
            _ => 0,
        }
    }
}

impl ArrayData {
    pub fn element(e: AliasedYaml) -> ArrayData {
        ArrayData::Element(e)
    }
    fn fits_conditions(&self, conditions: &Vec<Condition>) -> bool {
        match self {
            ArrayData::Element(e) => e.fits_conditions(conditions),
            _ => false,
        }
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
    fn for_hash<F, R>(&mut self, path: &YamlPath, f: &F, r: &R) -> usize
    where
        F: Fn(&mut HashElement) -> usize,
        R: Fn(&mut Yaml) -> usize;
    fn edit_hash_structure<F>(&mut self, path: &YamlPath, f: &F) -> usize
    where
        F: Fn(&mut Vec<HashData>, String, Option<usize>) -> usize;

    /// Insert AliasedYaml into a hash
    /// Returns the amount of insertions.
    /// Can be more than 1 when using indexes or all array elements etc
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize {
        let f = |data: &mut Vec<HashData>, key: String, key_index: Option<usize>| {
            let value = HashData::Element(HashElement {
                key,
                value: h.to_owned(),
            });
            if key_index.is_none() {
                data.push(value);
                1
            } else if let Some(index) = key_index {
                if overwrite {
                    data[index] = value;
                    1
                } else {
                    0
                }
            } else {
                0
            }
        };
        self.edit_hash_structure(path, &f)
    }
    fn remove_from_hash(&mut self, path: &YamlPath) -> usize {
        let f = |data: &mut Vec<HashData>, _key: String, key_index: Option<usize>| {
            if let Some(index) = key_index {
                data.remove(index);
                1
            } else {
                0
            }
        };
        self.edit_hash_structure(path, &f)
    }
    fn rename_field(&mut self, path: &YamlPath, new_name: String) -> usize {
        let f = |e: &mut HashElement| {
            e.key = new_name.clone();
            1
        };
        let r = |_hash: &mut Yaml| 0;
        self.for_hash(&path, &f, &r)
    }
    fn to_object(&mut self, path: &YamlPath, object_key: String) -> usize {
        let f = |e: &mut HashElement| {
            let val = e.value.clone();
            let new = AliasedYaml {
                alias: None,
                value: Yaml::Hash(vec![HashData::Element(HashElement {
                    key: object_key.clone(),
                    value: val,
                })]),
            };
            e.value = new;
            1
        };
        let r = |_hash: &mut Yaml| 0;
        self.for_hash(&path, &f, &r)
    }
    fn move_to_subfield(
        &mut self,
        path: &YamlPath,
        subfield: String,
        fields_to_move: Vec<String>,
    ) -> usize {
        let r = |hash: &mut Yaml| match hash {
            Yaml::Hash(_data) => {
                let fields: Vec<_> = fields_to_move
                    .iter()
                    .filter_map(|key| hash.key_value_owned(key).map(|val| (key, val)))
                    .collect();
                if !fields.is_empty() {
                    let subfield_value_opt = hash.key_value_mut(&subfield);
                    let res = if let Some(subfield_value) = subfield_value_opt {
                        for (key, value) in fields.iter() {
                            let path = key.parse().unwrap();
                            subfield_value.insert_into_hash(&path, &value, true);
                        }
                        1
                    } else {
                        let value = Yaml::Hash(
                            fields
                                .clone()
                                .into_iter()
                                .map(|(key, value)| {
                                    HashData::Element(HashElement {
                                        key: key.clone(),
                                        value: value.clone(),
                                    })
                                })
                                .collect(),
                        );
                        let new = AliasedYaml { alias: None, value };
                        hash.insert_into_hash(&subfield.parse().unwrap(), &new, false);
                        1
                    };
                    for (key, _) in fields {
                        hash.remove_from_hash(&key.parse().unwrap());
                    }
                    res
                } else {
                    0
                }
            }
            _ => 0,
        };
        let f = |e: &mut HashElement| {
            let hash = &mut e.value.value;
            r(hash)
        };

        self.for_hash(&path, &f, &r)
    }
}

pub trait YamlTypes {
    fn as_string(&self) -> Option<String>;
}

impl YamlTypes for Yaml {
    fn as_string(&self) -> Option<String> {
        match self {
            Yaml::QuotedString(s) | Yaml::UnquotedString(s) => Some(s.clone()),
            Yaml::FoldedString(qs) => Some(parse_folded(qs.iter().map(|q| &q[..]).collect())),
            Yaml::LiteralString(qs) => Some(parse_literal(qs.iter().map(|q| &q[..]).collect())),
            // TODO: anchor?
            _ => None,
        }
    }
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
    fn key_value(&self, key: &String) -> Option<&AliasedYaml> {
        match self {
            Yaml::Hash(data) => data.iter().enumerate().find_map(|(_, v)| match v {
                HashData::Element(e) => {
                    if &e.key == key {
                        Some(&e.value)
                    } else {
                        None
                    }
                }
                _ => None,
            }),
            _ => None,
        }
    }
    fn key_value_owned(&self, key: &String) -> Option<AliasedYaml> {
        match self {
            Yaml::Hash(data) => data.iter().enumerate().find_map(|(_, v)| match v {
                HashData::Element(e) => {
                    if &e.key == key {
                        Some(e.value.clone())
                    } else {
                        None
                    }
                }
                _ => None,
            }),
            _ => None,
        }
    }
    fn key_value_mut(&mut self, key: &String) -> Option<&mut AliasedYaml> {
        match self {
            Yaml::Hash(data) => data.iter_mut().enumerate().find_map(|(_, v)| match v {
                HashData::Element(e) => {
                    if &e.key == key {
                        Some(&mut e.value)
                    } else {
                        None
                    }
                }
                _ => None,
            }),
            _ => None,
        }
    }
    fn fits_conditions(&self, conditions: &Vec<Condition>) -> bool {
        conditions
            .iter()
            .find(|c| {
                let index_opt = self.key_value(&c.field);
                if let Some(yaml) = index_opt {
                    if let Some(s) = yaml.as_string() {
                        s != c.value // invalid, not equal to filter
                    } else {
                        true // invalid, field is not a string
                    }
                } else {
                    true // invalid, field not found
                }
            })
            .is_none()
    }
}
impl YamlInsert for Yaml {
    fn for_hash<F, R>(&mut self, path: &YamlPath, f: &F, r: &R) -> usize
    where
        F: Fn(&mut HashElement) -> usize,
        R: Fn(&mut Yaml) -> usize,
    {
        match path {
            YamlPath::Root(conditions) => {
                if !self.fits_conditions(conditions) {
                    return 0;
                }
                r(self)
            }
            YamlPath::Key(k, conditions, _) => {
                if !self.fits_conditions(conditions) {
                    return 0;
                }
                let key_exists = self.key_index(&k);
                match self {
                    Yaml::Hash(data) => {
                        if let Some(index) = key_exists {
                            data[index].for_hash(path, f, r)
                        } else {
                            0
                        }
                    }
                    _ => 0,
                }
            }
            YamlPath::Indexes(indexes, conditions, _) => match self {
                Yaml::InlineArray(elements) => {
                    let mut count = 0;
                    for index in indexes.iter() {
                        let element_opt = elements.get_mut(*index);
                        if let Some(element) = element_opt {
                            if element.fits_conditions(conditions) {
                                count += element.for_hash(path, f, r)
                            }
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
                            if element.fits_conditions(conditions) {
                                count += (*element).for_hash(path, f, r)
                            }
                        }
                    }
                    count
                }
                _ => 0,
            },
            //YamlPath::AllIndexes(Some(other_path)) => match self {
            YamlPath::AllIndexes(conditions, _) => match self {
                Yaml::InlineArray(elements) => {
                    let mut count = 0;
                    for element in elements.iter_mut() {
                        if element.fits_conditions(conditions) {
                            count += element.for_hash(path, f, r)
                        }
                    }
                    count
                }
                Yaml::Array(elements) => {
                    let mut count = 0;
                    for element in elements.iter_mut() {
                        if element.fits_conditions(conditions) {
                            count += element.for_hash(path, f, r)
                        }
                    }
                    count
                }
                _ => 0,
            },
        }
    }

    fn edit_hash_structure<F>(&mut self, path: &YamlPath, f: &F) -> usize
    where
        F: Fn(&mut Vec<HashData>, String, Option<usize>) -> usize,
    {
        match path {
            YamlPath::Root(_) => 0, // can't change structure of root
            YamlPath::Key(k, conditions, other_path_opt) => {
                if !self.fits_conditions(conditions) {
                    return 0;
                }
                let key_exists = self.key_index(&k);
                match self {
                    Yaml::Hash(data) => {
                        if let Some(other_path) = other_path_opt {
                            if let Some(index) = key_exists {
                                data[index].edit_hash_structure(&*other_path, f)
                            } else {
                                0
                            }
                        } else {
                            f(data, k.clone(), key_exists)
                        }
                    }
                    _ => 0,
                }
            }
            YamlPath::Indexes(indexes, conditions, Some(other_path)) => match self {
                Yaml::InlineArray(elements) => {
                    let mut count = 0;
                    for index in indexes.iter() {
                        let element_opt = elements.get_mut(*index);
                        if let Some(element) = element_opt {
                            if element.fits_conditions(conditions) {
                                count += element.edit_hash_structure(&*other_path, f)
                            }
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
                            if element.fits_conditions(conditions) {
                                count += (*element).edit_hash_structure(&*other_path, f)
                            }
                        }
                    }
                    count
                }
                _ => 0,
            },
            YamlPath::AllIndexes(conditions, Some(other_path)) => match self {
                Yaml::InlineArray(elements) => {
                    let mut count = 0;
                    for element in elements.iter_mut() {
                        if element.fits_conditions(conditions) {
                            count += element.edit_hash_structure(&*other_path, f)
                        }
                    }
                    count
                }
                Yaml::Array(elements) => {
                    let mut count = 0;
                    for element in elements.iter_mut() {
                        match element {
                            ArrayData::Element(element) => {
                                if element.fits_conditions(conditions) {
                                    count += element.edit_hash_structure(&*other_path, f)
                                }
                            }
                            _ => (),
                        }
                    }
                    count
                }
                _ => 0,
            },
            YamlPath::Indexes(_, _, None) | YamlPath::AllIndexes(_, None) => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AliasedYaml {
    pub alias: Option<String>,
    pub value: Yaml,
}

impl AliasedYaml {
    fn fits_conditions(&self, conditions: &Vec<Condition>) -> bool {
        self.value.fits_conditions(conditions)
    }
}

impl YamlTypes for AliasedYaml {
    fn as_string(&self) -> Option<String> {
        self.value.as_string()
    }
}

impl YamlInsert for AliasedYaml {
    fn edit_hash_structure<F>(&mut self, path: &YamlPath, f: &F) -> usize
    where
        F: Fn(&mut Vec<HashData>, String, Option<usize>) -> usize,
    {
        self.value.edit_hash_structure(path, f)
    }
    fn for_hash<F, R>(&mut self, path: &YamlPath, f: &F, r: &R) -> usize
    where
        F: Fn(&mut HashElement) -> usize,
        R: Fn(&mut Yaml) -> usize,
    {
        self.value.for_hash(path, f, r)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DocumentData {
    Comment(String),
    Yaml(Yaml),
}

impl YamlInsert for DocumentData {
    fn edit_hash_structure<F>(&mut self, path: &YamlPath, f: &F) -> usize
    where
        F: Fn(&mut Vec<HashData>, String, Option<usize>) -> usize,
    {
        match self {
            DocumentData::Yaml(y) => y.edit_hash_structure(path, f),
            _ => 0,
        }
    }
    fn for_hash<F, R>(&mut self, path: &YamlPath, f: &F, r: &R) -> usize
    where
        F: Fn(&mut HashElement) -> usize,
        R: Fn(&mut Yaml) -> usize,
    {
        match self {
            DocumentData::Yaml(y) => y.for_hash(path, f, r),
            _ => 0,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Document {
    items: Vec<DocumentData>,
}

impl YamlInsert for Document {
    fn edit_hash_structure<F>(&mut self, path: &YamlPath, f: &F) -> usize
    where
        F: Fn(&mut Vec<HashData>, String, Option<usize>) -> usize,
    {
        let mut count = 0;
        for item in self.items.iter_mut() {
            count += item.edit_hash_structure(path, f);
        }
        count
    }
    fn for_hash<F, R>(&mut self, path: &YamlPath, f: &F, r: &R) -> usize
    where
        F: Fn(&mut HashElement) -> usize,
        R: Fn(&mut Yaml) -> usize,
    {
        let mut count = 0;
        for item in self.items.iter_mut() {
            count += item.for_hash(path, f, r);
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
                    element.format(f, spaces, None)?;
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
    use super::{AliasedYaml, Yaml, YamlInsert, YamlPath};

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
    fn block_scalars_deeper() {
        let inp = r#"---
parts:
  - 
    type: gapfill
    marks: 0
    prompt:
      content: test
      other: |
        8
        10"#;

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

    #[test]
    fn rename_field() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key".parse().unwrap();
        assert_eq!(1, parsed.rename_field(&path, "newkey".to_string()));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 newkey: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }

    #[test]
    fn rename_field_list_in_list() {
        let inp = r#"---
parts:
  - type: gapfill
    gaps:
      - type: jme
        old_name: value
      - type: jme
        old_name: value
  - type: random 
    gaps:
      - type: jme
        old_name: value
      - type: jme
        old_name: value
  - type: gapfill
    gaps:
      - type: jme
        old_name: value
      - type: jme
        old_name: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "parts[*]|type=gapfill.gaps[*].old_name".parse().unwrap();
        assert_eq!(4, parsed.rename_field(&path, "new_name".to_string()));

        let out = r#"---
parts:
  - type: gapfill
    gaps:
      - type: jme
        new_name: value
      - type: jme
        new_name: value
  - type: random 
    gaps:
      - type: jme
        old_name: value
      - type: jme
        old_name: value
  - type: gapfill
    gaps:
      - type: jme
        new_name: value
      - type: jme
        new_name: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }

    #[test]
    fn to_object() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key".parse().unwrap();
        assert_eq!(1, parsed.to_object(&path, "subkey".to_string()));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: 
   subkey: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn to_object_with_condition() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key|key=value".parse().unwrap();
        assert_eq!(1, parsed.to_object(&path, "subkey".to_string()));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: 
   subkey: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key|key=other".parse().unwrap();
        assert_eq!(0, parsed.to_object(&path, "subkey".to_string()));
        assert_eq!(parse_yaml_file(inp).unwrap(), parsed);
    }
    #[test]
    fn remove_from_hash() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item".parse().unwrap();
        assert_eq!(1, parsed.remove_from_hash(&path));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn remove_from_hash_deeper() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key:
  item: 5
  item2: 6
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key.item".parse().unwrap();
        assert_eq!(1, parsed.remove_from_hash(&path));
        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
  key:
    item2: 6
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn remove_from_hash_deeper_with_condition() {
        let inp = r#"---
type: test
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key:
  item: 5
  item2: 6
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item|type=test.key.item".parse().unwrap();
        assert_eq!(1, parsed.remove_from_hash(&path));
        let out = r#"---
type: test
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
  key:
    item2: 6
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item|type=other.key.item".parse().unwrap();
        assert_eq!(0, parsed.remove_from_hash(&path));
        assert_eq!(parse_yaml_file(inp).unwrap(), parsed);
    }
    #[test]
    fn move_to_subfield_in_root() {
        let inp = r#"---
a: vala
b: valb
c: valc
d: vald
e: vale
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(
                &path,
                "sub".to_string(),
                vec!["e".to_string(), "b".to_string(), "d".to_string()]
            )
        );
        let out = r#"---
a: vala
c: valc
sub:
    e: vale
    b: valb
    d: vald
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_subfield() {
        let inp = r#"---
k:
  a: vala
  b: valb
  c: valc
  d: vald
  e: vale
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(
                &path,
                "sub".to_string(),
                vec!["e".to_string(), "b".to_string(), "d".to_string()]
            )
        );
        let out = r#"---
k:
  a: vala
  c: valc
  sub:
    e: vale
    b: valb
    d: vald
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_existing_subfield() {
        let inp = r#"---
k:
  a: vala
  b: valb
  c: valc
  d: vald
  e: vale
  l:
    z: valz
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(
                &path,
                "l".to_string(),
                vec![
                    "e".to_string(),
                    "b".to_string(),
                    "d".to_string(),
                    "kk".to_string()
                ]
            )
        );
        let out = r#"---
k:
  a: vala
  c: valc
  l:
    z: valz
    e: vale
    b: valb
    d: vald
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_subfield_list() {
        let inp = r#"---
k:
  - item1: test0
    result:
      yes: three
  - item1: test1
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k[*]".parse().unwrap();
        assert_eq!(
            2,
            parsed.move_to_subfield(&path, "result".to_string(), vec!["item1".to_string(),])
        );
        let out = r#"---
k:
  - result:
      yes: three
      item1: test0
  - result:
      item1: test1
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_subfield_list_item() {
        let inp = r#"---
k:
  - item1: test0
    result:
      yes: three
  - item1: test1
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k[0]".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(&path, "result".to_string(), vec!["item1".to_string(),])
        );
        let out = r#"---
k:
  - result:
      yes: three
      item1: test0
  - item1: test1
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_subfield_list_filter() {
        let inp = r#"---
k:
  - item1: test0
    result:
      yes: three
  - item1: test1
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k[*]|item1=test1".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(&path, "result".to_string(), vec!["item1".to_string(),])
        );
        let out = r#"---
k:
  - item1: test0
    result:
      yes: three      
  - result:
      item1: test1
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
}
