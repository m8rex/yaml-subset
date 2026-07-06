use super::insert::Additive;
use super::YamlInsert;
use super::{AliasedYaml, HashData, HashElement, Yaml};
use crate::path::Condition;
use crate::yaml::{MapYaml, Pretty, VisitYaml};
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
    fn for_hash<F, R, A: Additive>(&mut self, path: &YamlPath, f: &F, r: &R) -> A
    where
        F: Fn(&mut HashElement) -> A,
        R: Fn(&mut Yaml) -> A,
    {
        match self {
            ArrayData::Element(a) => match path {
                YamlPath::Root(_conditions) => A::zero(),
                YamlPath::Key(_key, _conditions, None) => a.for_hash(path, f, r),
                YamlPath::AllIndexes(_, Some(other)) => a.for_hash(&*other, f, r),
                YamlPath::AllIndexes(_, None) => a.for_hash(&YamlPath::Root(Vec::new()), f, r),
                YamlPath::Indexes(_, _, Some(other)) => a.for_hash(&*other, f, r),
                YamlPath::Indexes(_, _, None) => a.for_hash(&YamlPath::Root(Vec::new()), f, r),
                _ => A::zero(),
            },
            _ => A::zero(),
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

impl VisitYaml for ArrayData {
    fn visit_yaml<F: FnMut(&AliasedYaml)>(&self, f: &mut F) {
        if let ArrayData::Element(e) = self {
            e.visit_yaml(f);
        }
    }
}

impl MapYaml for ArrayData {
    fn map_yaml<F: FnMut(Yaml) -> Yaml>(self, f: &mut F) -> Self {
        match self {
            ArrayData::Element(e) => ArrayData::Element(e.map_yaml(f)),
            other => other,
        }
    }
}

impl Pretty for ArrayData {
    fn pretty_with_options(self, in_inline: bool) -> Self {
        match self {
            ArrayData::Element(e) => ArrayData::Element(e.pretty_with_options(in_inline)),
            o => o,
        }
    }
}
