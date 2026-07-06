use serde::ser;
use serde::Serialize;

use super::string::{
    DoubleQuotedStringEscapedChar, DoubleQuotedStringPart, SingleQuotedStringEscapedChar,
    SingleQuotedStringPart,
};
use super::{AliasedYaml, ArrayData, HashData, HashElement, Yaml};

// ─── Error ───────────────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct YamlSerializeError(pub String);

impl std::fmt::Display for YamlSerializeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for YamlSerializeError {}

impl ser::Error for YamlSerializeError {
    fn custom<T: std::fmt::Display>(msg: T) -> Self {
        YamlSerializeError(msg.to_string())
    }
}

// ─── Helpers ─────────────────────────────────────────────────────────────────

fn aliased(value: Yaml) -> AliasedYaml {
    AliasedYaml { alias: None, value }
}

// Returns true when a string cannot be written as a plain (unquoted) YAML scalar.
// Rules based on yaml_subset's grammar:
//   - start disallowed: [ ] { } " ' # > | newline
//   - anywhere disallowed: ": " (colon-space), # , newlines
// Also quotes strings that serde's untagged-enum Content mechanism misinterprets:
// untagged enum deserialization calls deserialize_any to collect into Content, and
// deserialize_any maps UnquotedString("2") → visit_i64(2) → Content::I64(2).
// serde's ContentDeserializer::deserialize_string then fails on Content::I64 because
// it has no integer-to-string coercion. Quoting ensures Content::String is produced
// so Vec<String> fields next to integer fields in the same struct both work.
fn needs_quoting(s: &str) -> bool {
    if s.is_empty() {
        return true;
    }
    if s.starts_with(' ') || s.ends_with(' ') {
        return true;
    }
    match s.chars().next().unwrap() {
        '[' | ']' | '{' | '}' | '"' | '\'' | '#' | '>' | '|' => return true,
        _ => {}
    }
    if s.contains(": ") || s.contains('#') || s.contains('\n') || s.contains('\r') {
        return true;
    }
    s.parse::<i64>().is_ok()
        || s.parse::<f64>().is_ok()
        || matches!(s, "true" | "false" | "null" | "~")
}

// Returns the most readable Yaml scalar for a Rust string:
//   - UnquotedString when the grammar allows it (no special chars)
//   - SingleQuotedString when quoting is needed but no newlines (clean, no escapes)
//   - DoubleQuotedString with EscapedChar nodes for \n and \r, so that pretty()
//     can convert them to a LiteralString block; long plain strings become FoldedString.
fn str_to_yaml(s: &str) -> Yaml {
    if !needs_quoting(s) {
        return Yaml::UnquotedString(s.to_string());
    }
    if !s.contains('\n') && !s.contains('\r') {
        // Prefer single quotes unless the string contains ' but no " — then double quotes
        // avoid the '' doubling.
        if s.contains('\'') && !s.contains('"') {
            // Double-quoted: ' is fine unescaped, only \ needs escaping here.
            let mut parts: Vec<DoubleQuotedStringPart> = Vec::new();
            let mut buf = String::new();
            for ch in s.chars() {
                if ch == '\\' {
                    if !buf.is_empty() {
                        parts.push(DoubleQuotedStringPart::String(std::mem::take(&mut buf)));
                    }
                    parts.push(DoubleQuotedStringPart::EscapedChar(
                        DoubleQuotedStringEscapedChar::Backslash,
                    ));
                } else {
                    buf.push(ch);
                }
            }
            if !buf.is_empty() {
                parts.push(DoubleQuotedStringPart::String(buf));
            }
            return Yaml::DoubleQuotedString(parts);
        }
        // Single-quoted: handles ", \, and other special chars literally; ' is doubled.
        let mut parts: Vec<SingleQuotedStringPart> = Vec::new();
        let mut buf = String::new();
        for ch in s.chars() {
            if ch == '\'' {
                if !buf.is_empty() {
                    parts.push(SingleQuotedStringPart::String(std::mem::take(&mut buf)));
                }
                parts.push(SingleQuotedStringPart::EscapedChar(
                    SingleQuotedStringEscapedChar::SingleQuote,
                ));
            } else {
                buf.push(ch);
            }
        }
        if !buf.is_empty() {
            parts.push(SingleQuotedStringPart::String(buf));
        }
        return Yaml::SingleQuotedString(parts);
    }
    // Double-quoted with EscapedChar nodes for newlines (and " and \ within).
    // pretty() converts EscapedChar-containing strings to LiteralString blocks.
    let mut parts: Vec<DoubleQuotedStringPart> = Vec::new();
    let mut buf = String::new();
    for ch in s.chars() {
        match ch {
            '\n' => {
                if !buf.is_empty() {
                    parts.push(DoubleQuotedStringPart::String(std::mem::take(&mut buf)));
                }
                parts.push(DoubleQuotedStringPart::EscapedChar(
                    DoubleQuotedStringEscapedChar::Newline,
                ));
            }
            '\r' => {
                if !buf.is_empty() {
                    parts.push(DoubleQuotedStringPart::String(std::mem::take(&mut buf)));
                }
                parts.push(DoubleQuotedStringPart::EscapedChar(
                    DoubleQuotedStringEscapedChar::CarriageReturn,
                ));
            }
            '"' => {
                if !buf.is_empty() {
                    parts.push(DoubleQuotedStringPart::String(std::mem::take(&mut buf)));
                }
                parts.push(DoubleQuotedStringPart::EscapedChar(
                    DoubleQuotedStringEscapedChar::Quote,
                ));
            }
            '\\' => {
                if !buf.is_empty() {
                    parts.push(DoubleQuotedStringPart::String(std::mem::take(&mut buf)));
                }
                parts.push(DoubleQuotedStringPart::EscapedChar(
                    DoubleQuotedStringEscapedChar::Backslash,
                ));
            }
            c => buf.push(c),
        }
    }
    if !buf.is_empty() {
        parts.push(DoubleQuotedStringPart::String(buf));
    }
    Yaml::DoubleQuotedString(parts)
}

fn yaml_to_key_string(yaml: Yaml) -> Result<String, YamlSerializeError> {
    use super::string::parse_double_quoted_string;
    match yaml {
        Yaml::UnquotedString(s) => Ok(s),
        Yaml::DoubleQuotedString(ref parts) => Ok(parse_double_quoted_string(parts)),
        _ => Err(YamlSerializeError("non-string map key".to_string())),
    }
}

// ─── Main Serializer ─────────────────────────────────────────────────────────

pub struct YamlSerializer;

impl YamlSerializer {
    pub fn new() -> Self {
        Self
    }
}

impl ser::Serializer for YamlSerializer {
    type Ok = Yaml;
    type Error = YamlSerializeError;
    type SerializeSeq = SeqSerializer;
    type SerializeTuple = TupleSerializer;
    type SerializeTupleStruct = TupleSerializer;
    type SerializeTupleVariant = TupleSerializer;
    type SerializeMap = MapSerializer;
    type SerializeStruct = MapSerializer;
    type SerializeStructVariant = MapSerializer;

    fn serialize_bool(self, v: bool) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(
            if v { "true" } else { "false" }.to_string(),
        ))
    }
    fn serialize_i8(self, v: i8) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_i16(self, v: i16) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_i32(self, v: i32) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_i64(self, v: i64) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_i128(self, v: i128) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_u8(self, v: u8) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_u16(self, v: u16) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_u32(self, v: u32) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_u64(self, v: u64) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_u128(self, v: u128) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_f32(self, v: f32) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_f64(self, v: f64) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString(v.to_string()))
    }
    fn serialize_char(self, v: char) -> Result<Yaml, Self::Error> {
        Ok(str_to_yaml(&v.to_string()))
    }
    fn serialize_str(self, v: &str) -> Result<Yaml, Self::Error> {
        Ok(str_to_yaml(v))
    }
    fn serialize_bytes(self, v: &[u8]) -> Result<Yaml, Self::Error> {
        Ok(Yaml::Array(
            v.iter()
                .map(|b| ArrayData::Element(aliased(Yaml::UnquotedString(b.to_string()))))
                .collect(),
        ))
    }
    fn serialize_none(self) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString("~".to_string()))
    }
    fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Yaml, Self::Error> {
        value.serialize(Self)
    }
    fn serialize_unit(self) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString("~".to_string()))
    }
    fn serialize_unit_struct(self, _name: &'static str) -> Result<Yaml, Self::Error> {
        Ok(Yaml::UnquotedString("~".to_string()))
    }
    fn serialize_unit_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Yaml, Self::Error> {
        Ok(str_to_yaml(variant))
    }
    fn serialize_newtype_struct<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Yaml, Self::Error> {
        value.serialize(self)
    }
    fn serialize_newtype_variant<T: Serialize + ?Sized>(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        value: &T,
    ) -> Result<Yaml, Self::Error> {
        value.serialize(self)
    }
    fn serialize_seq(self, _len: Option<usize>) -> Result<SeqSerializer, Self::Error> {
        Ok(SeqSerializer {
            elements: Vec::new(),
        })
    }
    fn serialize_tuple(self, _len: usize) -> Result<TupleSerializer, Self::Error> {
        Ok(TupleSerializer {
            elements: Vec::new(),
        })
    }
    fn serialize_tuple_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<TupleSerializer, Self::Error> {
        self.serialize_tuple(len)
    }
    fn serialize_tuple_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        len: usize,
    ) -> Result<TupleSerializer, Self::Error> {
        self.serialize_tuple(len)
    }
    fn serialize_map(self, _len: Option<usize>) -> Result<MapSerializer, Self::Error> {
        Ok(MapSerializer {
            entries: Vec::new(),
            current_key: None,
        })
    }
    fn serialize_struct(
        self,
        _name: &'static str,
        len: usize,
    ) -> Result<MapSerializer, Self::Error> {
        self.serialize_map(Some(len))
    }
    fn serialize_struct_variant(
        self,
        _name: &'static str,
        _variant_index: u32,
        _variant: &'static str,
        len: usize,
    ) -> Result<MapSerializer, Self::Error> {
        self.serialize_map(Some(len))
    }
}

// ─── Sequence Serializer ─────────────────────────────────────────────────────

// Sequences produce a block Array. pretty() will auto-inline them if all
// elements turn out to be simple scalars that fit on one line.
pub struct SeqSerializer {
    elements: Vec<Yaml>,
}

impl ser::SerializeSeq for SeqSerializer {
    type Ok = Yaml;
    type Error = YamlSerializeError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.elements.push(value.serialize(YamlSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Yaml, Self::Error> {
        Ok(Yaml::Array(
            self.elements
                .into_iter()
                .map(|y| ArrayData::Element(aliased(y)))
                .collect(),
        ))
    }
}

// ─── Tuple Serializer ────────────────────────────────────────────────────────

// Rust tuples always become InlineArray so the serde_yaml 0.8 block-indent
// bug (where [name, [single_arg], "True"] was emitted as [name, [single_arg,
// True]]) cannot occur.  pretty() will also auto-inline the child sequences
// (e.g. the argument list) when they contain only simple scalars.
pub struct TupleSerializer {
    elements: Vec<Yaml>,
}

impl ser::SerializeTuple for TupleSerializer {
    type Ok = Yaml;
    type Error = YamlSerializeError;

    fn serialize_element<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        self.elements.push(value.serialize(YamlSerializer)?);
        Ok(())
    }

    fn end(self) -> Result<Yaml, Self::Error> {
        Ok(Yaml::InlineArray(self.elements))
    }
}

impl ser::SerializeTupleStruct for TupleSerializer {
    type Ok = Yaml;
    type Error = YamlSerializeError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<Yaml, Self::Error> {
        ser::SerializeTuple::end(self)
    }
}

impl ser::SerializeTupleVariant for TupleSerializer {
    type Ok = Yaml;
    type Error = YamlSerializeError;

    fn serialize_field<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        ser::SerializeTuple::serialize_element(self, value)
    }

    fn end(self) -> Result<Yaml, Self::Error> {
        ser::SerializeTuple::end(self)
    }
}

// ─── Map / Struct Serializer ─────────────────────────────────────────────────

pub struct MapSerializer {
    entries: Vec<(String, Yaml)>,
    current_key: Option<String>,
}

impl ser::SerializeMap for MapSerializer {
    type Ok = Yaml;
    type Error = YamlSerializeError;

    fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
        let yaml = key.serialize(YamlSerializer)?;
        self.current_key = Some(yaml_to_key_string(yaml)?);
        Ok(())
    }

    fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
        let key = self
            .current_key
            .take()
            .expect("serialize_value called without serialize_key");
        self.entries.push((key, value.serialize(YamlSerializer)?));
        Ok(())
    }

    fn end(self) -> Result<Yaml, Self::Error> {
        Ok(Yaml::Hash(
            self.entries
                .into_iter()
                .map(|(k, v)| {
                    HashData::Element(HashElement {
                        key: k,
                        value: aliased(v),
                    })
                })
                .collect(),
        ))
    }
}

impl ser::SerializeStruct for MapSerializer {
    type Ok = Yaml;
    type Error = YamlSerializeError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        self.entries
            .push((key.to_string(), value.serialize(YamlSerializer)?));
        Ok(())
    }

    fn end(self) -> Result<Yaml, Self::Error> {
        ser::SerializeMap::end(self)
    }
}

impl ser::SerializeStructVariant for MapSerializer {
    type Ok = Yaml;
    type Error = YamlSerializeError;

    fn serialize_field<T: Serialize + ?Sized>(
        &mut self,
        key: &'static str,
        value: &T,
    ) -> Result<(), Self::Error> {
        ser::SerializeStruct::serialize_field(self, key, value)
    }

    fn end(self) -> Result<Yaml, Self::Error> {
        ser::SerializeMap::end(self)
    }
}
