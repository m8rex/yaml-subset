mod aliased;
mod array_data;
mod document;
mod hash_data;
mod hash_element;
mod insert;
mod parser;
pub mod string;
mod types;
#[cfg(feature = "serde")]
pub mod serializer;
#[cfg(feature = "serde")]
pub mod deserializer;

pub use aliased::AliasedYaml;
pub use array_data::ArrayData;
pub use document::{Document, DocumentData};
pub use hash_data::HashData;
pub use hash_element::HashElement;
pub use insert::{MyVec, YamlInsert};
//pub use old_parser::parse_yaml_file;
pub use parser::{parse_yaml_file, DocumentResult, YamlError, YamlResult};
pub use types::YamlTypes;
#[cfg(feature = "serde")]
pub use serializer::YamlSerializer;
#[cfg(feature = "serde")]
pub use deserializer::{from_yaml, from_yaml_str, YamlDeserializer, YamlDeserializeError};

use crate::path::Condition;
use crate::utils::indent;
use crate::yaml::insert::Additive;
use crate::yaml::string::parse_double_quoted_string;
use crate::yaml::string::parse_single_quoted_string;
use crate::yaml::string::{create_folded, create_literal};
use crate::YamlPath;
use std::fmt::Write;

use aliased::Parent;
pub use string::DoubleQuotedStringPart;
pub use string::SingleQuotedStringPart;

#[derive(Debug, Clone, PartialEq)]
pub enum Yaml {
    InlineHash(Vec<(String, Yaml)>),
    Hash(Vec<HashData>),
    InlineArray(Vec<Yaml>),
    Array(Vec<ArrayData>),
    SingleQuotedString(Vec<SingleQuotedStringPart>),
    DoubleQuotedString(Vec<DoubleQuotedStringPart>),
    UnquotedString(String),
    FoldedString(Vec<String>, BlockChomping),
    LiteralString(Vec<String>, BlockChomping),
    Anchor(String),
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
    fn for_hash<F, R, A: Additive>(&mut self, path: &YamlPath, f: &F, r: &R) -> A
    where
        F: Fn(&mut HashElement) -> A,
        R: Fn(&mut Yaml) -> A,
    {
        match path {
            YamlPath::Root(conditions) => {
                if !self.fits_conditions(conditions) {
                    return A::zero();
                }
                r(self)
            }
            YamlPath::Key(k, conditions, _) => {
                if !self.fits_conditions(conditions) {
                    return A::zero();
                }
                let key_exists = self.key_index(&k);
                match self {
                    Yaml::Hash(data) => {
                        if let Some(index) = key_exists {
                            data[index].for_hash(path, f, r)
                        } else {
                            A::zero()
                        }
                    }
                    _ => A::zero(),
                }
            }
            YamlPath::Indexes(indexes, conditions, _) => match self {
                Yaml::InlineArray(elements) => {
                    let mut count = A::zero();
                    for index in indexes.iter() {
                        let element_opt = elements.get_mut(*index);
                        if let Some(element) = element_opt {
                            if element.fits_conditions(conditions) {
                                count = count + element.for_hash(path, f, r)
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
                    let mut count = A::zero();
                    for index in indexes.iter() {
                        let element_opt = elements.get_mut(*index);
                        if let Some(element) = element_opt {
                            if element.fits_conditions(conditions) {
                                count = count + (*element).for_hash(path, f, r)
                            }
                        }
                    }
                    count
                }
                _ => A::zero(),
            },
            //YamlPath::AllIndexes(Some(other_path)) => match self {
            YamlPath::AllIndexes(conditions, _) => match self {
                Yaml::InlineArray(elements) => {
                    let mut count = A::zero();
                    for element in elements.iter_mut() {
                        if element.fits_conditions(conditions) {
                            count = count + element.for_hash(path, f, r)
                        }
                    }
                    count
                }
                Yaml::Array(elements) => {
                    let mut count = A::zero();
                    for element in elements.iter_mut() {
                        if element.fits_conditions(conditions) {
                            count = count + element.for_hash(path, f, r)
                        }
                    }
                    count
                }
                _ => A::zero(),
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

impl Yaml {
    pub fn format(
        &self,
        f: &mut String,
        spaces: usize,
        parent: Option<Parent>,
    ) -> std::fmt::Result {
        match self {
            Yaml::InlineHash(v) => {
                write!(f, " {{")?;
                for (idx, element) in v.iter().enumerate() {
                    if idx > 0 {
                        write!(f, ",")?;
                    }
                    write!(f, " {}:", element.0)?;
                    element.1.format(f, 0, None)?;
                }
                if !v.is_empty() {
                    write!(f, " ")?;
                }
                write!(f, "}}")
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
                        write!(f, ",")?;
                    }
                    element.format(f, 0, None)?;
                }
                if !v.is_empty() {
                    write!(f, " ")?;
                }
                write!(f, "]")
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
            Yaml::DoubleQuotedString(parts) => {
                write!(f, " \"")?;
                for part in parts.iter() {
                    match part {
                        DoubleQuotedStringPart::String(s) => {
                            write!(f, "{}", s)?;
                        }
                        DoubleQuotedStringPart::EscapedChar(c) => {
                            write!(f, "\\{}", c.char())?;
                        }
                        DoubleQuotedStringPart::BlankLines(nb) => {
                            for _ in 0..(*nb + 1) {
                                writeln!(f, "")?;
                            }
                        }
                        DoubleQuotedStringPart::RemovableNewline => {
                            writeln!(f, "")?;
                        }
                    }
                }
                write!(f, "\"")
            }
            Yaml::SingleQuotedString(parts) => {
                write!(f, " '")?;
                for part in parts.iter() {
                    match part {
                        SingleQuotedStringPart::String(s) => {
                            write!(f, "{}", s)?;
                        }
                        SingleQuotedStringPart::EscapedChar(c) => {
                            write!(f, "'{}", c.char())?;
                        }
                        SingleQuotedStringPart::BlankLines(nb) => {
                            for _ in 0..(*nb + 1) {
                                writeln!(f, "")?;
                            }
                        }
                        SingleQuotedStringPart::RemovableNewline => {
                            writeln!(f, "")?;
                        }
                    }
                }
                write!(f, "'")
            }
            Yaml::LiteralString(lines, chomping) => {
                for (idx, line) in lines.iter().enumerate() {
                    if idx == 0 {
                        write!(f, " |")?;
                        chomping.write(f)?;
                        if line.starts_with(" ") {
                            write!(f, "2")?;
                        }
                    }
                    write!(f, "\n{}{}", indent(spaces), line)?;
                }
                Ok(())
            }
            Yaml::FoldedString(lines, chomping) => {
                for (idx, line) in lines.iter().enumerate() {
                    if idx == 0 {
                        write!(f, " >")?;
                        chomping.write(f)?;
                        if line.starts_with(" ") {
                            write!(f, "2")?;
                        }
                    }
                    write!(f, "\n{}{}", indent(spaces), line)?;
                }
                Ok(())
            }
            Yaml::Anchor(a) => write!(f, " *{}", a),
        }
    }
}

pub trait Pretty {
    fn pretty_with_options(self, in_inline: bool, child_of_array: bool) -> Self;
    fn pretty(self) -> Self
    where
        Self: Sized,
    {
        self.pretty_with_options(false, false)
    }
}

/// Apply a closure bottom-up to every [`Yaml`] node (children first, then the
/// current node). Implement this trait on container types to enable recursive
/// rewrites without manual match arms for every variant.
pub trait MapYaml {
    fn map_yaml<F: FnMut(Yaml) -> Yaml>(self, f: &mut F) -> Self;
}

impl<T: MapYaml> MapYaml for Vec<T> {
    fn map_yaml<F: FnMut(Yaml) -> Yaml>(self, f: &mut F) -> Self {
        self.into_iter().map(|x| x.map_yaml(f)).collect()
    }
}

impl MapYaml for Yaml {
    fn map_yaml<F: FnMut(Yaml) -> Yaml>(self, f: &mut F) -> Yaml {
        let descended = match self {
            Yaml::Hash(data) => Yaml::Hash(data.map_yaml(f)),
            Yaml::InlineHash(data) => Yaml::InlineHash(
                data.into_iter().map(|(k, v)| (k, v.map_yaml(f))).collect(),
            ),
            Yaml::Array(data) => Yaml::Array(data.map_yaml(f)),
            Yaml::InlineArray(data) => Yaml::InlineArray(data.map_yaml(f)),
            leaf => leaf,
        };
        f(descended)
    }
}

pub trait VisitYaml {
    fn visit_yaml<F: FnMut(&AliasedYaml)>(&self, f: &mut F);
}

impl<T: VisitYaml> VisitYaml for Vec<T> {
    fn visit_yaml<F: FnMut(&AliasedYaml)>(&self, f: &mut F) {
        for item in self {
            item.visit_yaml(f);
        }
    }
}

impl VisitYaml for Yaml {
    fn visit_yaml<F: FnMut(&AliasedYaml)>(&self, f: &mut F) {
        match self {
            Yaml::Hash(data) => data.visit_yaml(f),
            Yaml::InlineHash(data) => {
                for (_, v) in data {
                    v.visit_yaml(f);
                }
            }
            Yaml::Array(data) => data.visit_yaml(f),
            Yaml::InlineArray(data) => {
                for v in data {
                    v.visit_yaml(f);
                }
            }
            Yaml::SingleQuotedString(_)
            | Yaml::DoubleQuotedString(_)
            | Yaml::UnquotedString(_)
            | Yaml::FoldedString(_, _)
            | Yaml::LiteralString(_, _)
            | Yaml::Anchor(_) => {}
        }
    }
}

fn is_inlineable(yaml: &Yaml) -> bool {
    matches!(
        yaml,
        Yaml::UnquotedString(_)
            | Yaml::DoubleQuotedString(_)
            | Yaml::SingleQuotedString(_)
            | Yaml::InlineArray(_)
            | Yaml::InlineHash(_)
    )
}

impl<T: Pretty> Pretty for Vec<T> {
    fn pretty_with_options(self, in_inline: bool, child_of_array: bool) -> Self {
        self.into_iter().map(|x| x.pretty_with_options(in_inline, child_of_array)).collect()
    }
}

impl Pretty for Yaml {
    fn pretty_with_options(self, in_inline: bool, child_of_array: bool) -> Yaml {
        let max_line_length = 90;
        match self {
            Yaml::InlineHash(h) => Yaml::InlineHash(
                h.into_iter().map(|(k, v)| (k, v.pretty_with_options(true, false))).collect(),
            ),
            Yaml::Hash(v) => {
                if v.is_empty() {
                    Yaml::InlineHash(Vec::new())
                } else {
                    Yaml::Hash(v.pretty_with_options(false, false))
                }
            }
            Yaml::InlineArray(v) => Yaml::InlineArray(v.pretty_with_options(true, false)),
            Yaml::Array(v) => {
                if v.is_empty() {
                    return Yaml::InlineArray(Vec::new());
                }
                let prettied: Vec<ArrayData> = v.pretty_with_options(false, true);
                // Auto-inline only when this array is a direct child of another array
                // (block: child_of_array, or inline: in_inline).
                // Arrays that are direct values in a hash stay block.
                if child_of_array || in_inline {
                    let can_inline = prettied.iter().all(|d| match d {
                        ArrayData::Element(e) => e.alias.is_none() && is_inlineable(&e.value),
                        _ => false,
                    });
                    if can_inline {
                        let elems: Vec<Yaml> = prettied
                            .iter()
                            .filter_map(|d| match d {
                                ArrayData::Element(e) => Some(e.value.clone()),
                                _ => None,
                            })
                            .collect();
                        let candidate = Yaml::InlineArray(elems.clone());
                        let mut rendered = String::new();
                        if candidate.format(&mut rendered, 0, None).is_ok()
                            && rendered.len() <= max_line_length
                        {
                            return Yaml::InlineArray(elems);
                        }
                    }
                }
                Yaml::Array(prettied)
            }
            Yaml::UnquotedString(s) => Yaml::UnquotedString(s),
            Yaml::DoubleQuotedString(parts) => {
                if in_inline {
                    return Yaml::DoubleQuotedString(parts);
                }
                let contains_escape = parts
                    .iter()
                    .any(|x| matches!(x, DoubleQuotedStringPart::EscapedChar(_)));
                let s = parse_double_quoted_string(&parts);
                let total_length = s.len();
                let chomping = if s.ends_with('\n') { BlockChomping::Keep } else { BlockChomping::Strip };
                if contains_escape {
                    Yaml::LiteralString(create_literal(s), chomping)
                } else if total_length > max_line_length {
                    Yaml::FoldedString(create_folded(s, max_line_length), chomping)
                } else {
                    Yaml::DoubleQuotedString(parts)
                }
            }
            Yaml::SingleQuotedString(parts) => {
                if in_inline {
                    return Yaml::SingleQuotedString(parts);
                }
                let contains_newline = parts
                    .iter()
                    .any(|x| matches!(x, SingleQuotedStringPart::BlankLines(_)));
                let s = parse_single_quoted_string(&parts);
                let total_length = s.len();
                let chomping = if s.ends_with('\n') { BlockChomping::Keep } else { BlockChomping::Strip };
                if contains_newline {
                    Yaml::LiteralString(create_literal(s), chomping)
                } else if total_length > max_line_length {
                    Yaml::FoldedString(create_folded(s, max_line_length), chomping)
                } else {
                    Yaml::SingleQuotedString(parts)
                }
            }
            Yaml::LiteralString(lines, chomping) => Yaml::LiteralString(lines, chomping),
            Yaml::FoldedString(lines, chomping) => Yaml::FoldedString(lines, chomping),
            Yaml::Anchor(a) => Yaml::Anchor(a),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum BlockChomping {
    Clip,
    Strip,
    Keep,
}

impl BlockChomping {
    fn write(&self, f: &mut String) -> std::fmt::Result {
        match self {
            Self::Clip => Ok(()),
            Self::Strip => write!(f, "-"),
            Self::Keep => write!(f, "+"),
        }
    }
}

impl Default for BlockChomping {
    fn default() -> Self {
        Self::Clip
    }
}
