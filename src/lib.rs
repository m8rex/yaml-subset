extern crate peg;
use linked_hash_map::LinkedHashMap;

#[derive(Debug, Clone, PartialEq)]
pub enum Yaml {
    Integer(isize),
    String(String),
    Hash(LinkedHashMap<String, Yaml>),
    Array(Vec<Yaml>),
}:

peg::parser! {
  grammar parser() for str {
    rule number() -> Yaml
      = n:$(['0'..='9']+) {? n.parse().map(|v| Yaml::Integer).or(Err("number")) }

    rule whitespace() = quiet!{[' ' | '\n' | '\t']+}

    pub rule inline_list() -> Vec<u32>
      = "[" l:(number() ** ",") "]" { l }

    rule string() -> String
        = 

    pub rule hash() -> Yaml
      = 

    pub rule document() -> Yaml
      = hash() / array()

    pub rule yaml() -> Yaml
      = "---" whitespace() x:document() { x }
  }
}

pub fn main() {
    assert_eq!(
        list_parser::list("[1,1,2,3,5,8]"),
        Ok(vec![1, 1, 2, 3, 5, 8])
    );
}

#[cfg(test)]
mod tests {
    use super::parser;
    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }
}
