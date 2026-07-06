use super::insert::Additive;
use super::YamlInsert;
use super::{AliasedYaml, HashElement, Yaml};
use crate::yaml::{MapYaml, Pretty, VisitYaml};
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

impl VisitYaml for HashData {
    fn visit_yaml<F: FnMut(&AliasedYaml)>(&self, f: &mut F) {
        if let HashData::Element(e) = self {
            e.value.visit_yaml(f);
        }
    }
}

impl MapYaml for HashData {
    fn map_yaml<F: FnMut(Yaml) -> Yaml>(self, f: &mut F) -> Self {
        match self {
            HashData::Element(e) => HashData::Element(e.map_yaml(f)),
            other => other,
        }
    }
}

impl Pretty for HashData {
    fn pretty_with_options(self, in_inline: bool, child_of_array: bool) -> Self {
        match self {
            HashData::Element(e) => HashData::Element(e.pretty_with_options(in_inline, child_of_array)),
            o => o,
        }
    }
}
