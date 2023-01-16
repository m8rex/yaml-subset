use super::YamlInsert;
use super::{AliasedYaml, HashData, HashElement, Yaml};
use crate::path::Condition;
use crate::YamlPath;

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
    pub fn fits_conditions(&self, conditions: &Vec<Condition>) -> bool {
        match self {
            ArrayData::Element(e) => e.fits_conditions(conditions),
            _ => false,
        }
    }
}
