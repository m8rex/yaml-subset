use pest_consume::{match_nodes, Error, Parser};
type PathResult<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(Parser)]
#[grammar = "path.pest"]
struct PathParser;

#[pest_consume::parser]
impl PathParser {
    fn EOI(_input: Node) -> PathResult<()> {
        Ok(())
    }
    fn pure_key(input: Node) -> PathResult<String> {
        Ok(input.as_str().to_string())
    }
    fn field(input: Node) -> PathResult<String> {
        Ok(input.as_str().to_string())
    }
    fn value(input: Node) -> PathResult<String> {
        Ok(input.as_str().to_string())
    }
    fn condition(input: Node) -> PathResult<Condition> {
        Ok(match_nodes!(input.into_children();
            [field(f), value(v)] => Condition { field: f, value: v }
        ))
    }
    fn key(input: Node) -> PathResult<YamlPath> {
        Ok(match_nodes!(input.into_children();
            [pure_key(key), condition(cs)..] => YamlPath::Key(key, cs.collect(), None)
        ))
    }

    fn index_number(input: Node) -> PathResult<Vec<usize>> {
        Ok(vec![input.as_str().parse().unwrap()]) // TODO: remove unwrap
    }
    fn index_all(input: Node) -> PathResult<()> {
        Ok(())
    }
    fn part(input: Node) -> PathResult<YamlPath> {
        Ok(match_nodes!(input.into_children();
            [index_all(_), condition(cs)..] => YamlPath::AllIndexes(cs.collect(), None),
            [index_number(indexes), condition(cs)..] => YamlPath::Indexes(indexes, cs.collect(), None),
            [key(r)] => r,

        ))
    }
    fn path(input: Node) -> PathResult<YamlPath> {
        Ok(match_nodes!(input.into_children();
            [key(s), part(vals)..] => {
                let mut path = s;
                for val in vals.into_iter() {
                    path.insert(val);
                }
                path
            }
        ))
    }
    fn root_key(_input: Node) -> PathResult<()> {
        Ok(())
    }
    fn root(input: Node) -> PathResult<YamlPath> {
        Ok(match_nodes!(input.into_children();
            [root_key(_s), condition(cs)..] => {
            YamlPath::Root(cs.collect())
            }
        ))
    }
    fn final_path(input: Node) -> PathResult<YamlPath> {
        Ok(match_nodes!(input.into_children();
            [path(p), EOI(_)] => p,
            [root(p), EOI(_)] => p
        ))
    }
}

fn parse(input_str: &str) -> PathResult<YamlPath> {
    // Parse the input into `Nodes`
    let inputs = PathParser::parse(Rule::final_path, input_str)?;
    // There should be a single root node in the parsed tree
    let input = inputs.single()?;
    // Consume the `Node` recursively into the final value
    PathParser::final_path(input)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Condition {
    pub field: String,
    pub value: String,
}

#[derive(Debug, Clone, PartialEq)]
pub enum YamlPath {
    Root(Vec<Condition>),
    Key(String, Vec<Condition>, Option<Box<YamlPath>>),
    AllIndexes(Vec<Condition>, Option<Box<YamlPath>>),
    Indexes(Vec<usize>, Vec<Condition>, Option<Box<YamlPath>>),
}

impl std::str::FromStr for YamlPath {
    type Err = pest_consume::Error<Rule>;

    fn from_str(path_str: &str) -> PathResult<Self> {
        parse(path_str)
    }
}

impl YamlPath {
    pub fn insert(&mut self, p: YamlPath) {
        match self {
            YamlPath::Root(_) => *self = p,
            YamlPath::Key(_, _, Some(ref mut o)) => o.insert(p),
            YamlPath::AllIndexes(_, Some(ref mut o)) => o.insert(p),
            YamlPath::Indexes(_, _, Some(ref mut o)) => o.insert(p),
            YamlPath::Key(_, _, ref mut o) => *o = Some(Box::new(p)),
            YamlPath::AllIndexes(_, ref mut o) => *o = Some(Box::new(p)),
            YamlPath::Indexes(_, _, ref mut o) => *o = Some(Box::new(p)),
        }
    }
}
#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn parse() {
        assert_eq!("".parse(), Ok(YamlPath::Root(Vec::new())));
        assert_eq!(
            "item".parse(),
            Ok(YamlPath::Key("item".to_string(), Vec::new(), None))
        );
        assert_eq!(
            "item.item2".parse(),
            Ok(YamlPath::Key(
                "item".to_string(),
                Vec::new(),
                Some(Box::new(YamlPath::Key(
                    "item2".to_string(),
                    Vec::new(),
                    None
                )))
            ))
        );
        assert_eq!(
            "item.item2.item3".parse(),
            Ok(YamlPath::Key(
                "item".to_string(),
                Vec::new(),
                Some(Box::new(YamlPath::Key(
                    "item2".to_string(),
                    Vec::new(),
                    Some(Box::new(YamlPath::Key(
                        "item3".to_string(),
                        Vec::new(),
                        None
                    )))
                )))
            ))
        );
    }

    #[test]
    fn parse_with_conditions() {
        assert_eq!(
            r#"item|field=test.item2"#.parse(),
            Ok(YamlPath::Key(
                "item".to_string(),
                vec![Condition {
                    field: "field".to_string(),
                    value: "test".to_string()
                }],
                Some(Box::new(YamlPath::Key(
                    "item2".to_string(),
                    Vec::new(),
                    None
                )))
            ))
        );
        assert_eq!(
            r#"item|field=test|other=something.item2|yeah=oh"#.parse(),
            Ok(YamlPath::Key(
                "item".to_string(),
                vec![
                    Condition {
                        field: "field".to_string(),
                        value: "test".to_string()
                    },
                    Condition {
                        field: "other".to_string(),
                        value: "something".to_string()
                    }
                ],
                Some(Box::new(YamlPath::Key(
                    "item2".to_string(),
                    vec![Condition {
                        field: "yeah".to_string(),
                        value: "oh".to_string()
                    }],
                    None
                )))
            ))
        );
        assert_eq!(
            r#"item[*]|field=test|other=something.item2|yeah=oh"#.parse(),
            Ok(YamlPath::Key(
                "item".to_string(),
                Vec::new(),
                Some(Box::new(YamlPath::AllIndexes(
                    vec![
                        Condition {
                            field: "field".to_string(),
                            value: "test".to_string()
                        },
                        Condition {
                            field: "other".to_string(),
                            value: "something".to_string()
                        }
                    ],
                    Some(Box::new(YamlPath::Key(
                        "item2".to_string(),
                        vec![Condition {
                            field: "yeah".to_string(),
                            value: "oh".to_string()
                        }],
                        None
                    )))
                )))
            ))
        );
    }

    #[test]
    fn insert() {
        let first: YamlPath = "minimum_marks".parse().unwrap();
        let mut new: YamlPath = "parts[*].gaps|type=gapfill[*]".parse().unwrap();
        new.insert(first);
        assert_eq!(
            new,
            "parts[*].gaps|type=gapfill[*].minimum_marks"
                .parse()
                .unwrap()
        );

        let first: YamlPath = "minimum_marks".parse().unwrap();
        let mut new: YamlPath = "parts[*]|type=gapfill.gaps[*]".parse().unwrap();
        new.insert(first);
        assert_eq!(
            new,
            "parts[*]|type=gapfill.gaps[*].minimum_marks"
                .parse()
                .unwrap()
        );
    }
}
