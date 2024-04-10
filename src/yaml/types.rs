use super::{
    string::{parse_double_quoted_string, parse_folded, parse_literal, parse_single_quoted_string},
    Yaml,
};

pub trait YamlTypes {
    fn as_string(&self) -> Option<String>;
}

impl YamlTypes for Yaml {
    fn as_string(&self) -> Option<String> {
        match self {
            Yaml::UnquotedString(s) => Some(s.clone()),
            Yaml::SingleQuotedString(s) => Some(parse_single_quoted_string(&s)),
            Yaml::DoubleQuotedString(s) => Some(parse_double_quoted_string(&s)),
            Yaml::FoldedString(qs, chomping) => {
                Some(parse_folded(qs.iter().map(|q| &q[..]).collect(), *chomping))
            }
            Yaml::LiteralString(qs, chomping) => Some(parse_literal(
                qs.iter().map(|q| &q[..]).collect(),
                *chomping,
            )),
            // TODO: anchor?
            _ => None,
        }
    }
}
