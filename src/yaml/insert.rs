use super::YamlTypes;
use super::{AliasedYaml, HashData, HashElement, Yaml};
use crate::YamlPath;
use std::collections::BTreeMap;
use std::ops::Fn;

pub trait Additive: std::ops::Add<Output = Self> + Sized {
    fn zero() -> Self;
}

impl Additive for usize {
    fn zero() -> Self {
        0
    }
}

#[derive(Debug, PartialEq)]
pub struct MyVec<T>(pub Vec<T>);

impl<T> std::ops::Add<MyVec<T>> for MyVec<T> {
    type Output = Self;
    fn add(self, rhs: MyVec<T>) -> Self::Output {
        Self(self.0.into_iter().chain(rhs.0.into_iter()).collect())
    }
}

impl<T> Additive for MyVec<T> {
    fn zero() -> Self {
        Self(Vec::new())
    }
}

pub trait YamlInsert {
    fn for_hash<F, R, A: Additive>(&mut self, path: &YamlPath, f: &F, r: &R) -> A
    where
        F: Fn(&mut HashElement) -> A,
        R: Fn(&mut Yaml) -> A;

    fn edit_hash_structure<F>(&mut self, path: &YamlPath, f: &F) -> usize
    where
        F: Fn(&mut Vec<HashData>, String, Option<usize>) -> usize;

    /// Find values
    fn find_values(&mut self, path: &YamlPath) -> MyVec<Yaml> {
        let f = |e: &mut HashElement| MyVec(vec![e.value.value.clone()]);
        let r = |hash: &mut Yaml| MyVec(vec![hash.clone()]);
        self.for_hash(&path, &f, &r)
    }

    /// Insert AliasedYaml into a hash
    /// Returns the amount of insertions.
    /// Can be more than 1 when using indexes or all array elements etc
    fn insert_into_hash(&mut self, path: &YamlPath, h: &AliasedYaml, overwrite: bool) -> usize {
        let f = |data: &mut Vec<HashData>, key: String, key_index: Option<usize>| {
            let value = HashData::Element(HashElement {
                key,
                value: h.to_owned(),
            });
            if key_index.is_none() {
                data.push(value);
                1
            } else if let Some(index) = key_index {
                if overwrite {
                    data[index] = value;
                    1
                } else {
                    0
                }
            } else {
                0
            }
        };
        self.edit_hash_structure(path, &f)
    }

    fn insert_into_hash_by_call<F>(&mut self, path: &YamlPath, h: F, overwrite: bool) -> usize
    where
        F: Fn() -> AliasedYaml,
    {
        let f = |data: &mut Vec<HashData>, key: String, key_index: Option<usize>| {
            let value = HashData::Element(HashElement { key, value: h() });
            if key_index.is_none() {
                data.push(value);
                1
            } else if let Some(index) = key_index {
                if overwrite {
                    data[index] = value;
                    1
                } else {
                    0
                }
            } else {
                0
            }
        };
        self.edit_hash_structure(path, &f)
    }
    fn remove_from_hash(&mut self, path: &YamlPath) -> usize {
        let f = |data: &mut Vec<HashData>, _key: String, key_index: Option<usize>| {
            if let Some(index) = key_index {
                data.remove(index);
                1
            } else {
                0
            }
        };
        self.edit_hash_structure(path, &f)
    }
    fn rename_field(&mut self, path: &YamlPath, new_name: String) -> usize {
        let f = |e: &mut HashElement| {
            e.key = new_name.clone();
            1
        };
        let r = |_hash: &mut Yaml| 0;
        self.for_hash(&path, &f, &r)
    }
    fn to_object(&mut self, path: &YamlPath, object_key: String) -> usize {
        let f = |e: &mut HashElement| {
            let val = e.value.clone();
            let new = AliasedYaml {
                alias: None,
                value: Yaml::Hash(vec![HashData::Element(HashElement {
                    key: object_key.clone(),
                    value: val,
                })]),
            };
            e.value = new;
            1
        };
        let r = |_hash: &mut Yaml| 0;
        self.for_hash(&path, &f, &r)
    }
    fn move_to_subfield(
        &mut self,
        path: &YamlPath,
        subfield: String,
        fields_to_move: Vec<String>,
    ) -> usize {
        let r = |hash: &mut Yaml| match hash {
            Yaml::Hash(_data) => {
                let fields: Vec<_> = fields_to_move
                    .iter()
                    .filter_map(|key| hash.key_value_owned(key).map(|val| (key, val)))
                    .collect();
                if !fields.is_empty() {
                    let subfield_value_opt = hash.key_value_mut(&subfield);
                    let res = if let Some(subfield_value) = subfield_value_opt {
                        for (key, value) in fields.iter() {
                            let path = key.parse().unwrap();
                            subfield_value.insert_into_hash(&path, &value, true);
                        }
                        1
                    } else {
                        let value = Yaml::Hash(
                            fields
                                .clone()
                                .into_iter()
                                .map(|(key, value)| {
                                    HashData::Element(HashElement {
                                        key: key.clone(),
                                        value: value.clone(),
                                    })
                                })
                                .collect(),
                        );
                        let new = AliasedYaml { alias: None, value };
                        hash.insert_into_hash(&subfield.parse().unwrap(), &new, false);
                        1
                    };
                    for (key, _) in fields {
                        hash.remove_from_hash(&key.parse().unwrap());
                    }
                    res
                } else {
                    0
                }
            }
            _ => 0,
        };
        let f = |e: &mut HashElement| {
            let hash = &mut e.value.value;
            r(hash)
        };

        self.for_hash(&path, &f, &r)
    }
    fn move_to_map_with_field_as_key(
        &mut self,
        path: &YamlPath,
        old_map_name: String,
        selector: String,
        new_map_name: String,
        keeps: Vec<String>,
    ) -> usize {
        let r = |hash: &mut Yaml| match hash {
            Yaml::Hash(_data) => {
                if let Some(result) = hash.key_value_owned(&old_map_name) {
                    if let Yaml::Hash(hash_data) = result.value {
                        let mut new_hashes: BTreeMap<String, Vec<HashData>> = BTreeMap::new();
                        let mut old_hashes = Vec::new();
                        let mut comments = Vec::new();
                        let mut last_added_to = None;
                        for mut hash_data in hash_data {
                            match hash_data {
                                HashData::Element(ref hash_element) => {
                                    if let Some(key) =
                                        hash_element.value.value.key_value_owned(&selector)
                                    {
                                        if let Some(s) = key.as_string() {
                                            if keeps.contains(&s) || s.is_empty() {
                                                hash_data
                                                    .remove_from_hash(&selector.parse().unwrap());
                                                old_hashes.extend(comments.clone());
                                                comments.clear();
                                                old_hashes.push(hash_data);
                                                last_added_to = None
                                            } else {
                                                let mut value = hash_element.value.clone();
                                                value.remove_from_hash(&selector.parse().unwrap());

                                                let new_hash_element =
                                                    HashData::Element(HashElement {
                                                        key: hash_element.key.clone(),
                                                        value,
                                                    });

                                                if new_hashes.contains_key(&s) {
                                                    let i = new_hashes.get_mut(&s).unwrap();
                                                    i.extend(comments.clone());
                                                    i.push(new_hash_element);
                                                    comments.clear();
                                                } else {
                                                    let mut items = comments.clone();
                                                    items.push(new_hash_element);
                                                    new_hashes.insert(s.clone(), items);
                                                    comments.clear();
                                                }
                                                last_added_to = Some(s);
                                            }
                                        } else {
                                            old_hashes.extend(comments.clone());
                                            comments.clear();
                                            old_hashes.push(hash_data);
                                            last_added_to = None
                                        }
                                    } else {
                                        old_hashes.extend(comments.clone());
                                        comments.clear();
                                        old_hashes.push(hash_data);
                                        last_added_to = None
                                    }
                                }
                                HashData::Comment(_) | HashData::InlineComment(_) => {
                                    comments.push(hash_data)
                                }
                            }
                        }
                        if let Some(s) = last_added_to {
                            new_hashes.get_mut(&s).unwrap().extend(comments);
                        } else {
                            old_hashes.extend(comments);
                        }
                        if !new_hashes.is_empty() {
                            let new = AliasedYaml {
                                alias: result.alias.clone(),
                                value: Yaml::Hash(
                                    new_hashes
                                        .into_iter()
                                        .map(|(key, value)| {
                                            HashData::Element(HashElement {
                                                key: key.clone(),
                                                value: AliasedYaml {
                                                    alias: None,
                                                    value: Yaml::Hash(value),
                                                },
                                            })
                                        })
                                        .collect(),
                                ),
                            };
                            hash.insert_into_hash(&new_map_name.parse().unwrap(), &new, false);
                            let old = AliasedYaml {
                                alias: result.alias.clone(),
                                value: Yaml::Hash(old_hashes),
                            };
                            hash.insert_into_hash(&old_map_name.parse().unwrap(), &old, true);
                            1
                        } else {
                            0
                        }
                    } else {
                        0
                    }
                } else {
                    0
                }
            }
            _ => 0,
        };
        let f = |e: &mut HashElement| {
            let hash = &mut e.value.value;
            r(hash)
        };

        self.for_hash(&path, &f, &r)
    }
}
