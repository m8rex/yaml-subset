mod aliased;
mod array_data;
mod document;
mod hash_data;
mod hash_element;
mod insert;
mod parser;
mod string;
mod types;

pub use aliased::AliasedYaml;
pub use array_data::ArrayData;
pub use document::{Document, DocumentData};
pub use hash_data::HashData;
pub use hash_element::HashElement;
pub use insert::YamlInsert;
//pub use old_parser::parse_yaml_file;
pub use parser::parse_yaml_file;
pub use types::YamlTypes;

use crate::path::Condition;
use crate::utils::indent;
use crate::YamlPath;
use std::fmt::Write;

use aliased::Parent;
pub use string::DoubleQuotedStringPart;
pub use string::SingleQuotedStringPart;

#[derive(Debug, Clone, PartialEq)]
pub enum Yaml {
    EmptyInlineHash,
    Hash(Vec<HashData>),
    InlineArray(Vec<Yaml>),
    Array(Vec<ArrayData>),
    SingleQuotedString(Vec<SingleQuotedStringPart>),
    DoubleQuotedString(Vec<DoubleQuotedStringPart>),
    UnquotedString(String),
    FoldedString(Vec<String>),
    LiteralString(Vec<String>),
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

impl Yaml {
    pub fn format(
        &self,
        f: &mut String,
        spaces: usize,
        parent: Option<Parent>,
    ) -> std::fmt::Result {
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
