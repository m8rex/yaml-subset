use super::insert::Additive;
use super::YamlInsert;
use super::{HashElement, Yaml};
use crate::yaml::Pretty;
use crate::YamlPath;

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
    fn for_hash<F, R, A: Additive>(&mut self, path: &YamlPath, f: &F, r: &R) -> A
    where
        F: Fn(&mut HashElement) -> A,
        R: Fn(&mut Yaml) -> A,
    {
        match self {
            HashData::Element(e) => e.for_hash(path, f, r),
            _ => A::zero(),
        }
    }
}

impl Pretty for HashData {
    fn pretty(self) -> Self {
        match self {
            HashData::Element(e) => HashData::Element(e.pretty()),
            o => o,
        }
    }
}
