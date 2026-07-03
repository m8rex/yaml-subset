use super::insert::Additive;
use super::YamlInsert;
use super::{AliasedYaml, HashData, Yaml};
use crate::yaml::{MapYaml, Pretty};
use crate::YamlPath;

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
    fn for_hash<F, R, A: Additive>(&mut self, path: &YamlPath, f: &F, r: &R) -> A
    where
        F: Fn(&mut HashElement) -> A,
        R: Fn(&mut Yaml) -> A,
    {
        match path {
            YamlPath::Root(_conditions) => f(self),
            YamlPath::Key(key, _conditions, None) => {
                if key == &self.key {
                    f(self)
                } else {
                    A::zero()
                }
            }
            YamlPath::Key(_key, _conditions, Some(other)) => self.value.for_hash(&*other, f, r),
            _ => A::zero(),
        }
    }
}

impl MapYaml for HashElement {
    fn map_yaml<F: FnMut(Yaml) -> Yaml>(self, f: &mut F) -> Self {
        Self { key: self.key, value: self.value.map_yaml(f) }
    }
}

impl Pretty for HashElement {
    fn pretty(self) -> Self {
        Self {
            key: self.key,
            value: self.value.pretty(),
        }
    }
}
