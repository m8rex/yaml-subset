use super::{HashData, HashElement, Yaml};
use super::{Pretty, YamlInsert};
use crate::YamlPath;
use std::fmt::Write;

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

impl Pretty for DocumentData {
    fn pretty(self) -> Self {
        match self {
            DocumentData::Comment(c) => DocumentData::Comment(c),
            DocumentData::Yaml(c) => DocumentData::Yaml(c.pretty()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Document {
    pub leading_comments: Vec<String>,
    pub items: Vec<DocumentData>,
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

impl Document {
    pub fn format(&self) -> Result<String, std::fmt::Error> {
        let mut s = String::new();
        for comment in self.leading_comments.iter() {
            writeln!(&mut s, "#{}", comment)?;
        }

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

impl Pretty for Document {
    fn pretty(self) -> Self {
        Self {
            leading_comments: self.leading_comments,
            items: self.items.pretty(),
        }
    }
}
