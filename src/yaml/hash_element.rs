use super::YamlInsert;
use super::{AliasedYaml, HashData, Yaml};
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
