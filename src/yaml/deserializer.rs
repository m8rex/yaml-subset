use serde::de::{self, DeserializeSeed, EnumAccess, Error as DeError, MapAccess, SeqAccess, VariantAccess, Visitor};
use serde::Deserialize;

use super::string::{
    parse_double_quoted_string, parse_folded, parse_literal, parse_single_quoted_string,
};
use super::{AliasedYaml, ArrayData, HashData, Yaml};
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

// ─── Error ───────────────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct YamlDeserializeError(pub String);

impl std::fmt::Display for YamlDeserializeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for YamlDeserializeError {}

impl de::Error for YamlDeserializeError {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        YamlDeserializeError(msg.to_string())
    }
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

fn yaml_to_string(yaml: &Yaml) -> Option<String> {
    match yaml {
        Yaml::UnquotedString(s) => Some(s.clone()),
        Yaml::DoubleQuotedString(parts) => Some(parse_double_quoted_string(parts)),
        Yaml::LiteralString(lines, chomping) => Some(parse_literal(
            lines.iter().map(|s| s.as_str()).collect(),
            *chomping,
        )),
        Yaml::FoldedString(lines, chomping) => Some(parse_folded(
            lines.iter().map(|s| s.as_str()).collect(),
            *chomping,
        )),
        Yaml::SingleQuotedString(parts) => Some(parse_single_quoted_string(parts)),
        Yaml::InlineHash(_)
        | Yaml::Hash(_)
        | Yaml::InlineArray(_)
        | Yaml::Array(_)
        | Yaml::Anchor(_) => None,
    }
}

type Anchors = Rc<RefCell<HashMap<String, Yaml>>>;

fn register(anchors: &Anchors, aliased: &AliasedYaml) {
    if let Some(name) = &aliased.alias {
        anchors.borrow_mut().insert(name.clone(), aliased.value.clone());
    }
}

fn resolve(anchors: &Anchors, name: &str) -> Result<Yaml, YamlDeserializeError> {
    anchors
        .borrow()
        .get(name)
        .cloned()
        .ok_or_else(|| YamlDeserializeError(format!("undefined anchor: {name}")))
}

fn hash_entries(anchors: &Anchors, data: Vec<HashData>) -> Vec<(String, Yaml)> {
    data.into_iter()
        .filter_map(|d| match d {
            HashData::Element(e) => {
                register(anchors, &e.value);
                Some((e.key, e.value.value))
            }
            HashData::InlineComment(_) | HashData::Comment(_) => None,
        })
        .collect()
}

fn seq_values(anchors: &Anchors, elements: Vec<ArrayData>) -> Vec<Yaml> {
    elements
        .into_iter()
        .filter_map(|d| match d {
            ArrayData::Element(e) => {
                register(anchors, &e);
                Some(e.value)
            }
            ArrayData::InlineComment(_) | ArrayData::Comment(_) => None,
        })
        .collect()
}

fn into_resolved(yaml: Yaml, anchors: Anchors) -> Result<(Yaml, Anchors), YamlDeserializeError> {
    match yaml {
        Yaml::Anchor(ref name) => {
            let resolved = resolve(&anchors, name)?;
            Ok((resolved, anchors))
        }
        other => Ok((other, anchors)),
    }
}

// ─── Main Deserializer ───────────────────────────────────────────────────────

pub struct YamlDeserializer {
    yaml: Yaml,
    anchors: Anchors,
}

impl YamlDeserializer {
    pub fn new(yaml: Yaml) -> Self {
        Self { yaml, anchors: Rc::new(RefCell::new(HashMap::new())) }
    }

    fn with(yaml: Yaml, anchors: Anchors) -> Self {
        Self { yaml, anchors }
    }
}

pub fn from_yaml<'de, T: Deserialize<'de>>(yaml: Yaml) -> Result<T, YamlDeserializeError> {
    T::deserialize(YamlDeserializer::new(yaml))
}

pub fn from_yaml_str<T: for<'de> Deserialize<'de>>(s: &str) -> Result<T, YamlDeserializeError> {
    use super::{parse_yaml_file, DocumentData};
    let doc = parse_yaml_file(s).map_err(|e| YamlDeserializeError(format!("{e}")))?;
    let yaml = doc
        .items
        .into_iter()
        .find_map(|item| match item {
            DocumentData::Yaml(y) => Some(y),
            _ => None,
        })
        .ok_or_else(|| YamlDeserializeError("no YAML content in document".to_string()))?;
    from_yaml(yaml)
}

impl<'de> de::Deserializer<'de> for YamlDeserializer {
    type Error = YamlDeserializeError;

    fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let anchors = self.anchors;
        match self.yaml {
            Yaml::UnquotedString(s) => {
                // null
                if matches!(s.as_str(), "~" | "null" | "Null" | "NULL") {
                    return visitor.visit_unit();
                }
                // bool (YAML 1.2: lowercase only)
                if s == "true" {
                    return visitor.visit_bool(true);
                }
                if s == "false" {
                    return visitor.visit_bool(false);
                }
                // integer
                if let Ok(i) = s.parse::<i64>() {
                    return visitor.visit_i64(i);
                }
                if let Ok(u) = s.parse::<u64>() {
                    return visitor.visit_u64(u);
                }
                // float
                if let Ok(f) = s.parse::<f64>() {
                    return visitor.visit_f64(f);
                }
                // string fallback (covers "True", "False", enum variant names, etc.)
                visitor.visit_string(s)
            }
            Yaml::DoubleQuotedString(ref parts) => {
                visitor.visit_string(parse_double_quoted_string(parts))
            }
            Yaml::LiteralString(ref lines, chomping) => visitor.visit_string(parse_literal(
                lines.iter().map(|s| s.as_str()).collect(),
                chomping,
            )),
            Yaml::FoldedString(ref lines, chomping) => visitor.visit_string(parse_folded(
                lines.iter().map(|s| s.as_str()).collect(),
                chomping,
            )),
            Yaml::SingleQuotedString(ref parts) => {
                visitor.visit_string(parse_single_quoted_string(parts))
            }
            Yaml::Array(elements) => {
                let values = seq_values(&anchors, elements);
                visitor.visit_seq(VecSeqAccess::new(values, anchors))
            }
            Yaml::InlineArray(elements) => visitor.visit_seq(VecSeqAccess::new(elements, anchors)),
            Yaml::Hash(data) => {
                let entries = hash_entries(&anchors, data);
                visitor.visit_map(VecMapAccess::new(entries, anchors))
            }
            Yaml::InlineHash(data) => visitor.visit_map(VecMapAccess::new(data, anchors)),
            Yaml::Anchor(name) => {
                let resolved = resolve(&anchors, &name)?;
                YamlDeserializer::with(resolved, anchors).deserialize_any(visitor)
            }
        }
    }

    fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let (yaml, _) = into_resolved(self.yaml, self.anchors)?;
        match yaml {
            Yaml::UnquotedString(ref s) => match s.as_str() {
                "true" | "True" | "TRUE" | "yes" | "Yes" | "YES" | "on" | "On" | "ON" => {
                    visitor.visit_bool(true)
                }
                "false" | "False" | "FALSE" | "no" | "No" | "NO" | "off" | "Off" | "OFF" => {
                    visitor.visit_bool(false)
                }
                _ => Err(Self::Error::custom(format!("expected bool, got {:?}", s))),
            },
            _ => Err(Self::Error::custom("expected bool")),
        }
    }

    fn deserialize_i8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_i64(visitor)
    }
    fn deserialize_i16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_i64(visitor)
    }
    fn deserialize_i32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_i64(visitor)
    }
    fn deserialize_i64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let (yaml, _) = into_resolved(self.yaml, self.anchors)?;
        if let Some(s) = yaml_to_string(&yaml) {
            s.parse::<i64>()
                .map_err(|e| Self::Error::custom(e))
                .and_then(|v| visitor.visit_i64(v))
        } else {
            Err(Self::Error::custom("expected integer"))
        }
    }
    fn deserialize_u8<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_u64(visitor)
    }
    fn deserialize_u16<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_u64(visitor)
    }
    fn deserialize_u32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_u64(visitor)
    }
    fn deserialize_u64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let (yaml, _) = into_resolved(self.yaml, self.anchors)?;
        if let Some(s) = yaml_to_string(&yaml) {
            s.parse::<u64>()
                .map_err(|e| Self::Error::custom(e))
                .and_then(|v| visitor.visit_u64(v))
        } else {
            Err(Self::Error::custom("expected unsigned integer"))
        }
    }
    fn deserialize_f32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_f64(visitor)
    }
    fn deserialize_f64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let (yaml, _) = into_resolved(self.yaml, self.anchors)?;
        if let Some(s) = yaml_to_string(&yaml) {
            s.parse::<f64>()
                .map_err(|e| Self::Error::custom(e))
                .and_then(|v| visitor.visit_f64(v))
        } else {
            Err(Self::Error::custom("expected float"))
        }
    }
    fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_str(visitor)
    }
    fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let (yaml, _) = into_resolved(self.yaml, self.anchors)?;
        if let Some(s) = yaml_to_string(&yaml) {
            visitor.visit_string(s)
        } else {
            Err(Self::Error::custom("expected string"))
        }
    }
    fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_str(visitor)
    }
    fn deserialize_bytes<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_seq(visitor)
    }
    fn deserialize_byte_buf<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let (yaml, anchors) = into_resolved(self.yaml, self.anchors)?;
        match &yaml {
            Yaml::UnquotedString(s) if matches!(s.as_str(), "~" | "null" | "Null" | "NULL") => {
                visitor.visit_none()
            }
            _ => visitor.visit_some(YamlDeserializer::with(yaml, anchors)),
        }
    }

    fn deserialize_unit<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_unit()
    }
    fn deserialize_unit_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        visitor.visit_unit()
    }

    fn deserialize_newtype_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        visitor.visit_newtype_struct(self)
    }

    fn deserialize_seq<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let (yaml, anchors) = into_resolved(self.yaml, self.anchors)?;
        match yaml {
            Yaml::Array(elements) => {
                let values = seq_values(&anchors, elements);
                visitor.visit_seq(VecSeqAccess::new(values, anchors))
            }
            Yaml::InlineArray(elements) => visitor.visit_seq(VecSeqAccess::new(elements, anchors)),
            _ => Err(Self::Error::custom("expected sequence")),
        }
    }

    fn deserialize_tuple<V: Visitor<'de>>(
        self,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        self.deserialize_seq(visitor)
    }

    fn deserialize_tuple_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        self.deserialize_tuple(len, visitor)
    }

    fn deserialize_map<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        let (yaml, anchors) = into_resolved(self.yaml, self.anchors)?;
        match yaml {
            Yaml::Hash(data) => {
                let entries = hash_entries(&anchors, data);
                visitor.visit_map(VecMapAccess::new(entries, anchors))
            }
            Yaml::InlineHash(data) => visitor.visit_map(VecMapAccess::new(data, anchors)),
            _ => Err(Self::Error::custom("expected map")),
        }
    }

    fn deserialize_struct<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        self.deserialize_map(visitor)
    }

    fn deserialize_enum<V: Visitor<'de>>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        let (yaml, anchors) = into_resolved(self.yaml, self.anchors)?;
        match yaml {
            // Unit variant: the yaml value is a string matching the variant name.
            // UnquotedString covers "True", "False", and bare identifiers.
            ref y @ (Yaml::UnquotedString(_)
            | Yaml::DoubleQuotedString(_)
            | Yaml::SingleQuotedString(_)
            | Yaml::LiteralString(_, _)
            | Yaml::FoldedString(_, _)) => {
                let s = yaml_to_string(y)
                    .ok_or_else(|| Self::Error::custom("expected string for enum variant"))?;
                visitor.visit_enum(StrEnumAccess {
                    variant: s,
                    value: Yaml::UnquotedString("~".to_string()),
                    anchors,
                })
            }
            // Newtype / tuple / struct variant: single-key map where the key is
            // the variant name and the value is the variant's content.
            Yaml::Hash(data) => {
                let mut entries = hash_entries(&anchors, data);
                if entries.len() == 1 {
                    let (variant, value) = entries.remove(0);
                    visitor.visit_enum(StrEnumAccess { variant, value, anchors })
                } else {
                    Err(Self::Error::custom(
                        "expected single-key map for tagged enum variant",
                    ))
                }
            }
            Yaml::InlineHash(mut data) => {
                if data.len() == 1 {
                    let (variant, value) = data.remove(0);
                    visitor.visit_enum(StrEnumAccess { variant, value, anchors })
                } else {
                    Err(Self::Error::custom(
                        "expected single-key inline hash for tagged enum variant",
                    ))
                }
            }
            Yaml::InlineArray(_) | Yaml::Array(_) | Yaml::Anchor(_) => {
                Err(Self::Error::custom("expected string or map for enum"))
            }
        }
    }

    fn deserialize_identifier<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
        visitor.visit_unit()
    }
}

// ─── Sequence Access ─────────────────────────────────────────────────────────

struct VecSeqAccess {
    iter: std::vec::IntoIter<Yaml>,
    anchors: Anchors,
}

impl VecSeqAccess {
    fn new(v: Vec<Yaml>, anchors: Anchors) -> Self {
        Self { iter: v.into_iter(), anchors }
    }
}

impl<'de> SeqAccess<'de> for VecSeqAccess {
    type Error = YamlDeserializeError;

    fn next_element_seed<T: DeserializeSeed<'de>>(
        &mut self,
        seed: T,
    ) -> Result<Option<T::Value>, Self::Error> {
        match self.iter.next() {
            Some(yaml) => seed.deserialize(YamlDeserializer::with(yaml, Rc::clone(&self.anchors))).map(Some),
            None => Ok(None),
        }
    }
}

// ─── Map Access ──────────────────────────────────────────────────────────────

struct VecMapAccess {
    iter: std::vec::IntoIter<(String, Yaml)>,
    pending_value: Option<Yaml>,
    anchors: Anchors,
}

impl VecMapAccess {
    fn new(v: Vec<(String, Yaml)>, anchors: Anchors) -> Self {
        Self {
            iter: v.into_iter(),
            pending_value: None,
            anchors,
        }
    }
}

impl<'de> MapAccess<'de> for VecMapAccess {
    type Error = YamlDeserializeError;

    fn next_key_seed<K: DeserializeSeed<'de>>(
        &mut self,
        seed: K,
    ) -> Result<Option<K::Value>, Self::Error> {
        match self.iter.next() {
            Some((k, v)) => {
                self.pending_value = Some(v);
                seed.deserialize(YamlDeserializer::with(Yaml::UnquotedString(k), Rc::clone(&self.anchors)))
                    .map(Some)
            }
            None => Ok(None),
        }
    }

    fn next_value_seed<V: DeserializeSeed<'de>>(
        &mut self,
        seed: V,
    ) -> Result<V::Value, Self::Error> {
        let v = self
            .pending_value
            .take()
            .expect("next_value_seed called without next_key_seed");
        seed.deserialize(YamlDeserializer::with(v, Rc::clone(&self.anchors)))
    }
}

// ─── Enum Access ─────────────────────────────────────────────────────────────

struct StrEnumAccess {
    variant: String,
    value: Yaml,
    anchors: Anchors,
}

impl<'de> EnumAccess<'de> for StrEnumAccess {
    type Error = YamlDeserializeError;
    type Variant = YamlDeserializer;

    fn variant_seed<V: DeserializeSeed<'de>>(
        self,
        seed: V,
    ) -> Result<(V::Value, YamlDeserializer), Self::Error> {
        let variant = seed.deserialize(YamlDeserializer::with(
            Yaml::UnquotedString(self.variant),
            Rc::clone(&self.anchors),
        ))?;
        Ok((variant, YamlDeserializer::with(self.value, self.anchors)))
    }
}

impl<'de> VariantAccess<'de> for YamlDeserializer {
    type Error = YamlDeserializeError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn newtype_variant_seed<T: DeserializeSeed<'de>>(
        self,
        seed: T,
    ) -> Result<T::Value, Self::Error> {
        seed.deserialize(self)
    }

    fn tuple_variant<V: Visitor<'de>>(
        self,
        len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        de::Deserializer::deserialize_tuple(self, len, visitor)
    }

    fn struct_variant<V: Visitor<'de>>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error> {
        de::Deserializer::deserialize_struct(self, "", fields, visitor)
    }
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use crate::yaml::serializer::YamlSerializer;
    use crate::yaml::{DoubleQuotedStringPart, Pretty};
    use serde::{Deserialize, Serialize};

    fn round_trip<T>(value: &T) -> T
    where
        T: Serialize + for<'de> Deserialize<'de> + std::fmt::Debug + PartialEq,
    {
        let yaml = value.serialize(YamlSerializer).unwrap().pretty();
        from_yaml(yaml).unwrap()
    }

    fn round_trip_check<T>(value: T)
    where
        T: Serialize + for<'de> Deserialize<'de> + std::fmt::Debug + PartialEq + Clone,
    {
        assert_eq!(value.clone(), round_trip(&value));
    }

    // ── Primitives ───────────────────────────────────────────────────────────

    #[test]
    fn bool_true() {
        round_trip_check(true);
    }
    #[test]
    fn bool_false() {
        round_trip_check(false);
    }
    #[test]
    fn integer_positive() {
        round_trip_check(42i64);
    }
    #[test]
    fn integer_negative() {
        round_trip_check(-7i64);
    }
    #[test]
    fn integer_zero() {
        round_trip_check(0i64);
    }
    #[test]
    fn unsigned() {
        round_trip_check(u64::MAX);
    }
    #[test]
    fn float() {
        round_trip_check(3.14f64);
    }
    #[test]
    fn string_simple() {
        round_trip_check("hello".to_string());
    }
    #[test]
    fn string_with_double_quote() {
        round_trip_check(r#"say "hello""#.to_string());
    }
    #[test]
    fn string_with_backslash() {
        round_trip_check("path\\to\\file".to_string());
    }
    #[test]
    fn string_multiline() {
        round_trip_check("line one\nline two\nline three\n".to_string());
    }
    #[test]
    fn string_long() {
        // FoldedString with Keep chomping always appends a trailing newline, so the
        // round-trip is lossless only for strings that already end with one.
        let s = format!("{}\n", "word ".repeat(38).trim_end()); // ~199 chars + newline
        round_trip_check(s);
    }
    #[test]
    fn option_some() {
        round_trip_check(Some(42i64));
    }
    #[test]
    fn option_none() {
        round_trip_check(Option::<i64>::None);
    }
    #[test]
    fn unit() {
        round_trip_check(());
    }

    // ── Collections ──────────────────────────────────────────────────────────

    #[test]
    fn vec_of_ints() {
        round_trip_check(vec![1i64, 2, 3, 4, 5]);
    }
    #[test]
    fn vec_of_strings() {
        round_trip_check(vec!["alpha".to_string(), "beta".to_string()]);
    }
    #[test]
    fn empty_vec() {
        round_trip_check(Vec::<i64>::new());
    }
    #[test]
    fn vec_stays_block_as_top_level_value() {
        let yaml = vec![1i64, 2, 3]
            .serialize(YamlSerializer)
            .unwrap()
            .pretty();
        assert_eq!(
            yaml,
            Yaml::Array(vec![
                ArrayData::Element(AliasedYaml { alias: None, value: Yaml::UnquotedString("1".to_string()) }),
                ArrayData::Element(AliasedYaml { alias: None, value: Yaml::UnquotedString("2".to_string()) }),
                ArrayData::Element(AliasedYaml { alias: None, value: Yaml::UnquotedString("3".to_string()) }),
            ]),
            "top-level vec should stay block"
        );
    }
    #[test]
    fn vec_stays_block_when_long() {
        let yaml = vec!["a".repeat(30), "b".repeat(30), "c".repeat(30)]
            .serialize(YamlSerializer)
            .unwrap()
            .pretty();
        assert!(
            matches!(yaml, Yaml::Array(_)),
            "long string vec should stay block, got {:?}",
            yaml
        );
    }

    // ── Tuples ───────────────────────────────────────────────────────────────

    #[test]
    fn tuple_is_always_inline() {
        let yaml = ("hello".to_string(), 42i64)
            .serialize(YamlSerializer)
            .unwrap();
        assert_eq!(
            yaml,
            Yaml::InlineArray(vec![
                Yaml::UnquotedString("hello".to_string()),
                Yaml::UnquotedString("42".to_string()),
            ])
        );
    }
    #[test]
    fn tuple_round_trip() {
        round_trip_check(("hello".to_string(), 42i64, true));
    }
    #[test]
    fn tuple_with_vec_arg() {
        // This is the shape of FunctionCallsData::Normal – the key bug case.
        // The vec of args must also become inline so the whole thing fits on
        // one line without the serde_yaml 0.8 indentation bug.
        let value = ("is_even".to_string(), vec![6i64], true);
        let yaml = value.serialize(YamlSerializer).unwrap().pretty();
        assert_eq!(
            yaml,
            Yaml::InlineArray(vec![
                Yaml::UnquotedString("is_even".to_string()),
                Yaml::InlineArray(vec![Yaml::UnquotedString("6".to_string())]),
                Yaml::UnquotedString("true".to_string()),
            ]),
            "inner vec should auto-inline and outer is always inline"
        );
        round_trip_check(value);
    }

    // ── Structs ──────────────────────────────────────────────────────────────

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    struct Point {
        x: i64,
        y: i64,
    }

    #[test]
    fn struct_round_trip() {
        round_trip_check(Point { x: 3, y: -1 });
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    struct Nested {
        name: String,
        point: Point,
        tags: Vec<String>,
    }

    #[test]
    fn nested_struct_round_trip() {
        round_trip_check(Nested {
            name: "origin".to_string(),
            point: Point { x: 0, y: 0 },
            tags: vec!["a".to_string(), "b".to_string()],
        });
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    struct WithOption {
        value: Option<i64>,
        label: Option<String>,
    }

    #[test]
    fn struct_with_option_some() {
        round_trip_check(WithOption {
            value: Some(7),
            label: Some("hello".to_string()),
        });
    }
    #[test]
    fn struct_with_option_none() {
        round_trip_check(WithOption {
            value: None,
            label: None,
        });
    }

    // ── Enums ────────────────────────────────────────────────────────────────

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum Color {
        Red,
        Green,
        Blue,
    }

    #[test]
    fn unit_enum_round_trip() {
        round_trip_check(Color::Red);
        round_trip_check(Color::Green);
        round_trip_check(Color::Blue);
    }

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    #[serde(untagged)]
    enum UntaggedVal {
        Int(i64),
        Text(String),
        List(Vec<i64>),
    }

    #[test]
    fn untagged_int() {
        round_trip_check(UntaggedVal::Int(42));
    }
    #[test]
    fn untagged_list() {
        round_trip_check(UntaggedVal::List(vec![1, 2, 3]));
    }

    // ── PythonBool analogue ───────────────────────────────────────────────────
    // Mirrors the real PythonBool enum in yaml-types.  "True"/"False" are unit
    // variant names that must survive the fix_python_bools round-trip (where the
    // double-quoted "True" becomes unquoted True in the YAML output).

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    enum PyBool {
        True,
        False,
    }

    #[test]
    fn python_bool_true_round_trip() {
        round_trip_check(PyBool::True);
    }
    #[test]
    fn python_bool_false_round_trip() {
        round_trip_check(PyBool::False);
    }

    // Simulate what fix_python_bools does: "True" quoted → True unquoted.
    // The deserializer must handle UnquotedString("True") as the string "True".
    #[test]
    fn python_bool_from_unquoted() {
        let yaml = Yaml::UnquotedString("True".to_string());
        let result: PyBool = from_yaml(yaml).unwrap();
        assert_eq!(result, PyBool::True);
    }

    // ── FunctionCallsData analogue (the core serde_yaml bug) ─────────────────

    #[derive(Serialize, Deserialize, Debug, PartialEq, Clone)]
    #[serde(untagged)]
    enum FnCallData {
        WithSeed(String, Vec<i64>, PyBool, Option<usize>),
        Normal(String, Vec<i64>, PyBool),
    }

    #[test]
    fn function_call_normal_round_trip() {
        round_trip_check(FnCallData::Normal(
            "is_even".to_string(),
            vec![6],
            PyBool::True,
        ));
    }
    #[test]
    fn function_call_with_seed_round_trip() {
        round_trip_check(FnCallData::WithSeed(
            "double".to_string(),
            vec![3],
            PyBool::False,
            Some(42),
        ));
    }

    // Verify the YAML for Normal is a single inline array with the right content
    // (not block-split, and PyBool::True serializes as the unquoted string "True").
    #[test]
    fn function_call_normal_is_inline() {
        let value = FnCallData::Normal("is_even".to_string(), vec![6], PyBool::True);
        let yaml = value.serialize(YamlSerializer).unwrap().pretty();
        assert_eq!(
            yaml,
            Yaml::InlineArray(vec![
                Yaml::UnquotedString("is_even".to_string()),
                Yaml::InlineArray(vec![Yaml::UnquotedString("6".to_string())]),
                Yaml::UnquotedString("True".to_string()),
            ]),
            "FnCallData::Normal should be a 3-element InlineArray"
        );
    }

    // ── Anchors ───────────────────────────────────────────────────────────────

    #[derive(Deserialize, Debug, PartialEq)]
    struct ChooseOne {
        options: Vec<String>,
        solution: String,
    }

    #[test]
    fn anchor_in_array_resolved_by_alias() {
        let yaml_str = "---
options:
  - &correct answer_a
  - answer_b
solution: *correct
";
        let result: ChooseOne = from_yaml_str(yaml_str).unwrap();
        assert_eq!(result.solution, "answer_a");
        assert_eq!(result.options, vec!["answer_a", "answer_b"]);
    }

    #[test]
    fn undefined_anchor_returns_err() {
        let yaml_str = "---
solution: *missing
options: []";
        let result = from_yaml_str::<ChooseOne>(yaml_str);
        assert!(result.is_err());
        assert!(result.unwrap_err().0.contains("undefined anchor"));
    }

    // ── flatten + untagged + internally-tagged ───────────────────────────────

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct FlatMeta { id: u32 }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(tag = "type", rename_all = "snake_case")]
    enum FlatInner { Foo(FooData), Bar(BarData) }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct FooData { x: u32 }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct BarData { y: String }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    #[serde(untagged)]
    enum FlatOuter { A(FlatInner), B(FlatBData) }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct FlatBData { name: String }

    #[derive(Serialize, Deserialize, Debug, PartialEq)]
    struct FlatItem {
        metadata: FlatMeta,
        #[serde(flatten)]
        content: FlatOuter,
    }

    #[test]
    fn literal_block_preserves_inner_indent() {
        // Python code in literal blocks must preserve relative indentation and
        // the Clip (|) chomping must add exactly one trailing newline.
        #[derive(Deserialize, Debug, PartialEq)]
        struct S { solution: String }
        let yaml = "---
solution: |
  for i in range(5,21):
    print(i)
";
        let result: S = from_yaml_str(yaml).unwrap();
        assert_eq!(result.solution, "for i in range(5,21):\n  print(i)\n");
    }

    #[test]
    fn literal_block_keep_preserves_trailing_newlines() {
        // |+ must keep ALL trailing newlines (not just one).
        #[derive(Deserialize, Debug, PartialEq)]
        struct S { value: String }
        let yaml = "---
value: |+
  hello

";
        let result: S = from_yaml_str(yaml).unwrap();
        assert_eq!(result.value, "hello\n\n");
    }

    #[test]
    fn literal_block_keep_multiple_trailing_blank_lines() {
        // |+ with two trailing blank lines must keep both.
        #[derive(Deserialize, Debug, PartialEq)]
        struct S { value: String, next: String }
        let yaml = "---
value: |+
  hello


next: done
";
        let result: S = from_yaml_str(yaml).unwrap();
        assert_eq!(result.value, "hello\n\n\n");
        assert_eq!(result.next, "done");
    }

    #[test]
    fn key_with_trailing_space_is_trimmed() {
        // yaml_subset must trim trailing whitespace from block mapping keys
        // so that "type : text" is treated the same as "type: text"
        let yaml = "---
type : foo
x: 7
";
        #[derive(Deserialize, Debug, PartialEq)]
        struct S { r#type: String, x: u32 }
        let result: Result<S, _> = from_yaml_str(yaml);
        assert_eq!(result.unwrap(), S { r#type: "foo".to_string(), x: 7 });
    }

    #[test]
    fn flatten_untagged_internally_tagged() {
        let yaml = "---
metadata:
  id: 42
type: foo
x: 7
";
        let result: Result<FlatItem, _> = from_yaml_str(yaml);
        assert_eq!(
            result.unwrap(),
            FlatItem { metadata: FlatMeta { id: 42 }, content: FlatOuter::A(FlatInner::Foo(FooData { x: 7 })) }
        );
    }

    // ── VisitYaml ────────────────────────────────────────────────────────────

    #[test]
    fn visit_yaml_collects_anchors() {
        use crate::yaml::{parse_yaml_file, DocumentData, VisitYaml};
        let yaml_str = "---
options:
  - &correct answer_a
  - &wrong answer_b
solution: *correct
";
        let doc = parse_yaml_file(yaml_str).unwrap();
        let yaml = doc.items.into_iter().find_map(|i| match i {
            DocumentData::Yaml(y) => Some(y),
            _ => None,
        }).unwrap();

        let mut names: Vec<String> = Vec::new();
        yaml.visit_yaml(&mut |a: &crate::yaml::AliasedYaml| {
            if let Some(name) = &a.alias {
                names.push(name.clone());
            }
        });
        names.sort();
        assert_eq!(names, vec!["correct", "wrong"]);
    }
}
