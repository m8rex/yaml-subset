use super::YamlInsert;
use super::YamlTypes;
use super::{HashData, HashElement, Yaml};
use crate::path::Condition;
use crate::yaml::Pretty;
use crate::YamlPath;
use std::fmt::Write;

#[derive(Debug, Clone, PartialEq)]
pub struct AliasedYaml {
    pub alias: Option<String>,
    pub value: Yaml,
}

impl AliasedYaml {
    pub fn fits_conditions(&self, conditions: &Vec<Condition>) -> bool {
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

#[derive(Debug, Clone, Copy)]
pub enum Parent {
    Hash,
    Array,
}

impl AliasedYaml {
    pub fn format(
        &self,
        f: &mut String,
        spaces: usize,
        parent: Option<Parent>,
    ) -> std::fmt::Result {
        if let Some(alias) = self.alias.as_ref() {
            write!(f, " &{}", alias)?;
            self.value.format(f, spaces + 2, None)
        } else {
            self.value.format(f, spaces + 2, parent)
        }
    }
}

impl Pretty for AliasedYaml {
    fn pretty(self) -> Self {
        Self {
            alias: self.alias,
            value: self.value.pretty(),
        }
    }
}
