use std::fmt::Display;

use super::{
    string::{
        DoubleQuotedStringEscapedChar, DoubleQuotedStringPart, SingleQuotedStringEscapedChar,
        SingleQuotedStringPart,
    },
    AliasedYaml, ArrayData, BlockChomping, Document, DocumentData, HashData, HashElement, Yaml,
};
use pest_consume::{match_nodes, Error, Parser};

#[derive(Debug)]
pub struct YamlError(Error<Rule>);

impl From<Error<Rule>> for YamlError {
    fn from(err: Error<Rule>) -> Self {
        YamlError(err)
    }
}

impl Display for YamlError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl YamlError {
    pub fn location(&self) -> (usize, usize) {
        match self.0.line_col {
            pest::error::LineColLocation::Pos((line, col)) => (line, col),
            pest::error::LineColLocation::Span((line, col), _) => (line, col),
        }
    }
}

pub type YamlResult<T> = std::result::Result<T, YamlError>;
type InternalYamlResult<T> = std::result::Result<T, Error<Rule>>;
pub type DocumentResult = YamlResult<Document>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(Parser)]
#[grammar = "yaml.pest"]
pub struct YamlParser;

#[pest_consume::parser]
impl YamlParser {
    fn EOI(_input: Node) -> InternalYamlResult<()> {
        Ok(())
    }

    fn comment_text(input: Node) -> InternalYamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn comment(input: Node) -> InternalYamlResult<String> {
        match_nodes!(input.into_children();
            [comment_text(inner)] => Ok(inner),
        )
    }

    fn commentnl(input: Node) -> InternalYamlResult<String> {
        match_nodes!(input.into_children();
            [comment_text(inner)] => Ok(inner),
        )
    }

    fn alias(input: Node) -> InternalYamlResult<String> {
        match_nodes!(input.into_children();
            [alias_name(inner)] => Ok(inner),
        )
    }

    fn alias_name(input: Node) -> InternalYamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn anchor(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [alias_name(inner)] => Ok(Yaml::Anchor(inner)),
        )
    }

    fn inline_hash_element(input: Node) -> InternalYamlResult<(String, Yaml)> {
        match_nodes!(input.into_children();
        [hash_key(k), inline_array_value(v)] => {

            Ok((k, v))
        })
    }

    fn inline_hash(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
        [inline_hash_element(v), inline_hash_element(vs)..] => {
            let mut values = vec![v];
            values.extend(vs);
            Ok(Yaml::InlineHash(values))
        },
        [] => Ok(Yaml::InlineHash(vec![])))
    }

    fn unquoted_string(input: Node) -> InternalYamlResult<Yaml> {
        Ok(Yaml::UnquotedString(
            input.as_str().to_string().trim_end_matches(" ").to_string(),
        ))
    }

    fn unquoted_inline_string(input: Node) -> InternalYamlResult<Yaml> {
        Ok(Yaml::UnquotedString(
            input.as_str().to_string().trim_end_matches(" ").to_string(),
        ))
    }

    fn escaped_double_quote_char_value(
        input: Node,
    ) -> InternalYamlResult<DoubleQuotedStringEscapedChar> {
        match input.as_str().chars().next().unwrap() {
            'n' => Ok(DoubleQuotedStringEscapedChar::Newline),
            '\n' => Ok(DoubleQuotedStringEscapedChar::RealNewline),
            'r' => Ok(DoubleQuotedStringEscapedChar::CarriageReturn),
            't' => Ok(DoubleQuotedStringEscapedChar::Tab),
            '\\' => Ok(DoubleQuotedStringEscapedChar::Backslash),
            '"' => Ok(DoubleQuotedStringEscapedChar::Quote),
            _ => unreachable!(),
        }
    }

    fn double_quote_text(input: Node) -> InternalYamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn removable_newline(_input: Node) -> InternalYamlResult<()> {
        Ok(())
    }

    fn blank_lines(input: Node) -> InternalYamlResult<usize> {
        Ok(input.as_str().lines().count() - 1)
    }

    fn double_quote_without_escape(input: Node) -> InternalYamlResult<DoubleQuotedStringPart> {
        match_nodes!(input.into_children();
            [escaped_double_quote_char_value(inner)] => Ok(DoubleQuotedStringPart::EscapedChar(inner)),
            [double_quote_text(inner)] => Ok(DoubleQuotedStringPart::String(inner)),
            [blank_lines(inner)] => Ok(DoubleQuotedStringPart::BlankLines(inner)),
            [removable_newline(_)] => Ok(DoubleQuotedStringPart::RemovableNewline),
        )
    }

    fn double_quote_content(input: Node) -> InternalYamlResult<Vec<DoubleQuotedStringPart>> {
        match_nodes!(input.into_children();
            [double_quote_without_escape(v)..] => Ok(v.collect()),
        )
    }

    fn double_quoted_string(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [double_quote_content(v)] => Ok(Yaml::DoubleQuotedString(v)),
        )
    }

    fn single_quote(input: Node) -> InternalYamlResult<SingleQuotedStringEscapedChar> {
        match input.as_str().chars().next().unwrap() {
            '\'' => Ok(SingleQuotedStringEscapedChar::SingleQuote),
            _ => unreachable!(),
        }
    }

    fn single_quote_text(input: Node) -> InternalYamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn escaped_single_quote_char(input: Node) -> InternalYamlResult<SingleQuotedStringPart> {
        match_nodes!(input.into_children();
            [single_quote(inner)] => Ok(SingleQuotedStringPart::EscapedChar(inner)),
            [single_quote_text(inner)] => Ok(SingleQuotedStringPart::String(inner)),
            [blank_lines(inner)] => Ok(SingleQuotedStringPart::BlankLines(inner)),
            [removable_newline(_)] => Ok(SingleQuotedStringPart::RemovableNewline),
        )
    }

    fn single_quote_content(input: Node) -> InternalYamlResult<Vec<SingleQuotedStringPart>> {
        match_nodes!(input.into_children();
            [escaped_single_quote_char(v)..] => Ok(v.collect()),
        )
    }

    fn single_quoted_string(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [single_quote_content(v)] => Ok(Yaml::SingleQuotedString(v)),
        )
    }

    fn inline_array_string(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [unquoted_inline_string(value)] => Ok(value),
            [double_quoted_string(value)] => Ok(value),
            [single_quoted_string(value)] => Ok(value))
    }

    fn inline_array_value(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [anchor(value)] => Ok(value),
            [inline_array(value)] => Ok(value),
            [inline_hash(value)] => Ok(value),
            [inline_array_string(value)] => Ok(value))
    }

    fn inline_array(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
        [inline_array_value(v), inline_array_value(vs)..] => {
            let mut values = vec![v];
            values.extend(vs);
            Ok(Yaml::InlineArray(values))
        },
        [] => Ok(Yaml::InlineArray(vec![])))
    }

    fn string(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [unquoted_string(value)] => Ok(value),
            [double_quoted_string(value)] => Ok(value),
            [single_quoted_string(value)] => Ok(value))
    }

    fn inline_value(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [anchor(value)] => Ok(value),
            [inline_array(value)] => Ok(value),
            [inline_hash(value)] => Ok(value),
            [string(value)] => Ok(value))
    }

    fn block_string(input: Node) -> InternalYamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn block_empty_lines(input: Node) -> InternalYamlResult<Vec<String>> {
        Ok((0..input.as_str().len())
            .into_iter()
            .map(|_| "".to_string())
            .collect())
    }

    fn string_block_chomping(input: Node) -> InternalYamlResult<BlockChomping> {
        Ok(match input.as_str() {
            "+" => BlockChomping::Keep,
            "-" => BlockChomping::Strip,
            _ => unreachable!(),
        })
    }

    fn string_multiline_indent(input: Node) -> InternalYamlResult<Option<usize>> {
        Ok(if input.as_str().is_empty() {
            None
        } else {
            let given: usize = input.as_str().parse().unwrap();
            Some(given - 1)
        })
    }

    fn string_multiline_content_part(input: Node) -> InternalYamlResult<Vec<String>> {
        match_nodes!(input.into_children();
        [block_empty_lines(es), block_string(b)] => {
            Ok(es.into_iter().chain(std::iter::once(b)).collect())
        })
    }

    fn string_multiline_content(input: Node) -> InternalYamlResult<Vec<String>> {
        match_nodes!(input.into_children();
        [block_empty_lines(es), block_string(b), string_multiline_content_part(bs)..] => {
            Ok(es.into_iter().chain(std::iter::once(b)).chain(bs.into_iter().flat_map(|x| x)).collect())
        })
    }

    fn string_multiline_folded(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
        [string_multiline_indent(indent), string_multiline_content(cs)] => Ok(Yaml::FoldedString(
            clean_multiline_strings(indent, cs), BlockChomping::Clip
        )),
        [string_block_chomping(chomp), string_multiline_indent(indent), string_multiline_content(cs)] => Ok(Yaml::FoldedString(
            clean_multiline_strings(indent, cs), chomp
        )))

        // string_block_chomping
    }

    fn string_multiline_literal(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
        [string_multiline_indent(indent), string_multiline_content(cs)] => Ok(Yaml::LiteralString(
            clean_multiline_strings(indent, cs), BlockChomping::Clip
        )),
        [string_block_chomping(chomp), string_multiline_indent(indent), string_multiline_content(cs)] => Ok(Yaml::LiteralString(
            clean_multiline_strings(indent, cs), chomp
        )))
    }

    fn yaml_value(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [hash(value)] => Ok(value),
            [array(value)] => Ok(value),
            [string_multiline_literal(value)] => Ok(value),
            [string_multiline_folded(value)] => Ok(value),
            [inline_value(value)] => Ok(value))
    }

    fn aliased_yaml_value(input: Node) -> InternalYamlResult<AliasedYaml> {
        match_nodes!(input.into_children();
        [alias(alias), yaml_value(val)] => Ok(AliasedYaml {
            alias: Some(alias),
            value: val,
        }),
        [yaml_value(val)] => Ok(AliasedYaml {
            alias: None,
            value: val,
        }))
    }

    fn block_array_aliased_yaml_value(input: Node) -> InternalYamlResult<AliasedYaml> {
        match_nodes!(input.into_children();
            [alternative_aliased_yaml_value(value)] => Ok(value),
            [aliased_yaml_value(value)] => Ok(value))
    }

    fn alternative_aliased_yaml_value(input: Node) -> InternalYamlResult<AliasedYaml> {
        match_nodes!(input.into_children();
        [alias(alias), alternative_hash(val)] => Ok(AliasedYaml {
            alias: Some(alias),
            value: val,
        }),
        [alternative_hash(val)] => Ok(AliasedYaml {
            alias: None,
            value: val,
        }),
        [alias(alias), alternative_array(val)] => Ok(AliasedYaml {
            alias: Some(alias),
            value: val,
        }),
        [alternative_array(val)] => Ok(AliasedYaml {
            alias: None,
            value: val,
        }))
    }

    fn hash_key(input: Node) -> InternalYamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn hash_element(input: Node) -> InternalYamlResult<HashElement> {
        //println!("{:#?}", input);
        match_nodes!(input.into_children();
                [hash_key(key), block_array_aliased_yaml_value(value)] => Ok(HashElement { key, value }),
        )
    }

    fn first_hash_element(input: Node) -> InternalYamlResult<Vec<HashData>> {
        //println!("{:#?}", input);

        match_nodes!(input.into_children();
            [hash_element(element), comment(c)] => Ok(vec![HashData::Element(element), HashData::InlineComment(c)]),
            [hash_element(element)] => Ok(vec![HashData::Element(element)]),

        )
    }

    fn hash_element_data(input: Node) -> InternalYamlResult<Vec<HashData>> {
        //println!("{:#?}", input);

        match_nodes!(input.into_children();
            [commentnls(cs), hash_element(element), comment(c)] => Ok(
                cs.into_iter().map(|c| HashData::Comment(c))
                .chain(vec![HashData::Element(element), HashData::InlineComment(c)].into_iter())
                .collect(),
            ),
            [commentnls(cs), hash_element(element)] => Ok(
                cs.into_iter().map(|c| HashData::Comment(c))
                .chain(vec![HashData::Element(element)].into_iter())
                .collect(),
            ),
        )
    }

    fn hash(input: Node) -> InternalYamlResult<Yaml> {
        //println!("{:#?}", input);
        match_nodes!(input.into_children();
            [commentnls(cs), first_hash_element(element1), hash_element_data(elements).., commentnls(cs2)] => Ok(
                Yaml::Hash(
                    cs.into_iter().map(|c| HashData::Comment(c))
                    .chain(element1.into_iter())
                    .chain(elements.into_iter().flatten())
                    .chain(cs2.into_iter().map(|c| HashData::Comment(c)))
                    .collect()
                )
            ),
        )
    }

    fn alternative_hash(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [first_hash_element(element1), commentnls(cs), first_hash_element(element2), hash_element_data(elements).., commentnls(cs2)] => Ok(
                Yaml::Hash(
                    element1.into_iter()
                    .chain(cs.into_iter().map(|c| HashData::Comment(c)))
                    .chain(element2.into_iter())
                    .chain(elements.into_iter().flatten())
                    .chain(cs2.into_iter().map(|c| HashData::Comment(c)))
                    .collect()
                )
            ),
        )
    }

    fn block_array_element(input: Node) -> InternalYamlResult<ArrayData> {
        match_nodes!(input.into_children();
            [block_array_aliased_yaml_value(value)] => Ok(ArrayData::Element(value)),
        )
    }

    fn first_block_array_element(input: Node) -> InternalYamlResult<Vec<ArrayData>> {
        match_nodes!(input.into_children();
            [block_array_element(element), comment(c)] => Ok(vec![element, ArrayData::InlineComment(c)]),
            [block_array_element(element)] => Ok(vec![element]),
        )
    }

    fn block_array_data(input: Node) -> InternalYamlResult<Vec<ArrayData>> {
        match_nodes!(input.into_children();
            [commentnls(cs), block_array_element(element), comment(c)] => Ok(
                cs.into_iter().map(|c| ArrayData::Comment(c))
                .chain(vec![element, ArrayData::InlineComment(c)].into_iter())
                .collect(),
            ),
            [commentnls(cs), block_array_element(element)] => Ok(
                cs.into_iter().map(|c| ArrayData::Comment(c))
                .chain(vec![element].into_iter())
                .collect(),
            ),
        )
    }

    fn array(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
            [inline_array(val)] => Ok(val),
            [block_array(val)] => Ok(val)
        )
    }

    fn block_array(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
        [commentnls(cs),first_block_array_element(element1), block_array_data(elements).., commentnls(cs2)] => Ok(
            Yaml::Array(
                cs.into_iter().map(|c| ArrayData::Comment(c))
                .chain(element1.into_iter())
                .chain(elements.into_iter().flatten())
                .chain(cs2.into_iter().map(|c| ArrayData::Comment(c)))
                .collect()
            )
        ))
    }

    fn alternative_array(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
        [first_block_array_element(element1),first_block_array_element(element2), block_array_data(elements).., commentnls(cs)] => Ok(
            Yaml::Array(
                element1.into_iter()
                .chain(element2.into_iter())
                .chain(elements.into_iter().flatten())
                .chain(cs.into_iter().map(|c| ArrayData::Comment(c)))
                .collect()
            )
        ))
    }

    fn commentnls(input: Node) -> InternalYamlResult<Vec<String>> {
        //println!("{:?}", input);
        match_nodes!(
            input.into_children();
            [commentnl(cs)..] => Ok(
                cs.collect()

            )
        )
    }

    fn leading_comments(input: Node) -> InternalYamlResult<Vec<String>> {
        //println!("{:?}", input);
        match_nodes!(
            input.into_children();
            [commentnl(cs)..] => Ok(
                cs.collect()

            )
        )
    }

    fn document(input: Node) -> InternalYamlResult<Yaml> {
        match_nodes!(input.into_children();
        [hash(h)] => Ok(h),
        [array(a)] => Ok(a))
    }

    fn yaml(input: Node) -> InternalYamlResult<Document> {
        //println!("{:#?}"  , input);
        match_nodes!(input.into_children();
        [leading_comments(before), commentnls(cs), document(val), comment(cs2).., EOI(_)] => Ok(
            Document {
                leading_comments: before,
                items: cs.into_iter().map(|c| DocumentData::Comment(c))
                .chain(vec![DocumentData::Yaml(val)].into_iter())
                .chain(cs2.into_iter().map(|c| DocumentData::Comment(c))).collect(),
            }
        ))
    }
}

pub fn parse_yaml_file(input_str: &str) -> YamlResult<Document> {
    // Parse the input into `Nodes`
    let inputs = YamlParser::parse(Rule::yaml, input_str)?;
    // There should be a single root node in the parsed tree
    let input = inputs.single()?;
    // Consume the `Node` recursively into the final value
    YamlParser::yaml(input).map_err(|e| e.into())
}

fn count_whitespace_bytes_at_start(input: &str) -> usize {
    input
        .chars()
        .take_while(|ch| ch.is_whitespace() && *ch != '\n')
        .map(|ch| ch.len_utf8())
        .sum()
}

fn clean_multiline_strings(indent: Option<usize>, lines: Vec<String>) -> Vec<String> {
    let indent = indent.unwrap_or_else(|| count_whitespace_bytes_at_start(&lines[0]));

    lines
        .into_iter()
        .map(|x| {
            if x.is_empty() {
                x
            } else {
                x[indent..].to_string()
            }
        })
        .collect()
}
