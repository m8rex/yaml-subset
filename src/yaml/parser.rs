use pest_consume::{match_nodes, Error, Parser};

use super::{AliasedYaml, Document, HashData, HashElement, Yaml, DocumentData, ArrayData, string::{DoubleQuotedStringPart, DoubleQuotedStringEscapedChar, SingleQuotedStringEscapedChar, SingleQuotedStringPart}};
type YamlResult<T> = std::result::Result<T, Error<Rule>>;
type Node<'i> = pest_consume::Node<'i, Rule, ()>;

#[derive(Parser)]
#[grammar = "yaml.pest"]
pub struct YamlParser;

#[pest_consume::parser]
impl YamlParser {
    fn EOI(_input: Node) -> YamlResult<()> {
        Ok(())
    }

    fn comment_text(input: Node) -> YamlResult<String> {
        Ok(input.as_str().to_string())       
    }

    fn comment(input: Node) -> YamlResult<String> {
        match_nodes!(input.into_children();
            [comment_text(inner)] => Ok(inner),
        )
    }

    fn commentnl(input: Node) -> YamlResult<String> {
        match_nodes!(input.into_children();
            [comment_text(inner)] => Ok(inner),
        )
    }

    fn alias(input: Node) -> YamlResult<String> {
        match_nodes!(input.into_children();
            [alias_name(inner)] => Ok(inner),
        )
    }

    fn alias_name(input: Node) -> YamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn anchor(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [alias_name(inner)] => Ok(Yaml::Anchor(inner)),
        )
    }

    fn inline_hash(_input: Node) -> YamlResult<Yaml> {
        Ok(Yaml::EmptyInlineHash)
    }

    fn unquoted_string(input: Node) -> YamlResult<Yaml> {
        Ok(Yaml::UnquotedString(input.as_str().to_string().trim_end_matches(" ").to_string()))
    }

    fn unquoted_inline_string(input: Node) -> YamlResult<Yaml> {
        Ok(Yaml::UnquotedString(input.as_str().to_string().trim_end_matches(" ").to_string()))
    }

    fn escaped_double_quote_char_value(input: Node) -> YamlResult<DoubleQuotedStringEscapedChar> {
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

    fn double_quote_text(input: Node) -> YamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn removable_newline(_input: Node) -> YamlResult<()> {
        Ok(())
    }

    fn blank_lines(input: Node) -> YamlResult<usize> {
        Ok(input.as_str().lines().count() - 1)
    }

    fn double_quote_without_escape(input: Node) -> YamlResult<DoubleQuotedStringPart> {
        match_nodes!(input.into_children();
            [escaped_double_quote_char_value(inner)] => Ok(DoubleQuotedStringPart::EscapedChar(inner)),
            [double_quote_text(inner)] => Ok(DoubleQuotedStringPart::String(inner)),
            [blank_lines(inner)] => Ok(DoubleQuotedStringPart::BlankLines(inner)),
            [removable_newline(_)] => Ok(DoubleQuotedStringPart::RemovableNewline),
        )
    }

    fn double_quote_content(input: Node) -> YamlResult<Vec<DoubleQuotedStringPart>> {
        match_nodes!(input.into_children();
            [double_quote_without_escape(v)..] => Ok(v.collect()),
        )
    }

    fn double_quoted_string(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [double_quote_content(v)] => Ok(Yaml::DoubleQuotedString(v)),
        )
    }

    fn single_quote(input: Node) -> YamlResult<SingleQuotedStringEscapedChar> {
        match input.as_str().chars().next().unwrap() {
            '\'' => Ok(SingleQuotedStringEscapedChar::SingleQuote),
            _ => unreachable!(),
        }
    }

    fn single_quote_text(input: Node) -> YamlResult<String> {
        Ok(input.as_str().to_string())
    }


    fn escaped_single_quote_char(input: Node) -> YamlResult<SingleQuotedStringPart> {
        match_nodes!(input.into_children();
            [single_quote(inner)] => Ok(SingleQuotedStringPart::EscapedChar(inner)),
            [single_quote_text(inner)] => Ok(SingleQuotedStringPart::String(inner)),
            [blank_lines(inner)] => Ok(SingleQuotedStringPart::BlankLines(inner)),
            [removable_newline(_)] => Ok(SingleQuotedStringPart::RemovableNewline),
        )
    }

    fn single_quote_content(input: Node) -> YamlResult<Vec<SingleQuotedStringPart>> {
        match_nodes!(input.into_children();
            [escaped_single_quote_char(v)..] => Ok(v.collect()),
        )
    }

    fn single_quoted_string(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [single_quote_content(v)] => Ok(Yaml::SingleQuotedString(v)),
        )
    }

    fn inline_array_string(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [unquoted_inline_string(value)] => Ok(value),
            [double_quoted_string(value)] => Ok(value),
            [single_quoted_string(value)] => Ok(value))
    }

    fn inline_array_value(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [anchor(value)] => Ok(value),
            [inline_array(value)] => Ok(value),
            [inline_hash(value)] => Ok(value),        
            [inline_array_string(value)] => Ok(value))
    }

    fn inline_array(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
        [inline_array_value(v), inline_array_value(vs)..] => {
            let mut values = vec![v];
            values.extend(vs);
            Ok(Yaml::Array(values.into_iter().map(|v| super::ArrayData::Element(AliasedYaml {
                alias: None,
                value: v,
            })).collect()))
        })
    }

    fn string(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [unquoted_string(value)] => Ok(value),
            [double_quoted_string(value)] => Ok(value),
            [single_quoted_string(value)] => Ok(value))
    }

    fn inline_value(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [anchor(value)] => Ok(value),
            [inline_array(value)] => Ok(value),
            [inline_hash(value)] => Ok(value),
            [string(value)] => Ok(value))
    }

    fn block_string(input: Node) -> YamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn string_multiline_content(input: Node) -> YamlResult<Vec<String>> {
        match_nodes!(input.into_children();
            [block_string(b), block_string(bs)..] => {
                Ok(vec![b].into_iter().chain(bs.into_iter()).collect())
            })
    }

    fn string_multiline_folded(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [string_multiline_content(cs)] => Ok(Yaml::FoldedString(cs)))
    }

    fn string_multiline_literal(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [string_multiline_content(cs)] => Ok(Yaml::LiteralString(cs)))
    }

    fn yaml_value(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [hash(value)] => Ok(value),
            [array(value)] => Ok(value),
            [string_multiline_literal(value)] => Ok(value),
            [string_multiline_folded(value)] => Ok(value),
            [inline_value(value)] => Ok(value))
    }

    fn aliased_yaml_value(input: Node) -> YamlResult<AliasedYaml> {
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

    fn block_array_aliased_yaml_value(input: Node) -> YamlResult<AliasedYaml> {
        match_nodes!(input.into_children();
            [alternative_aliased_yaml_value(value)] => Ok(value),
            [aliased_yaml_value(value)] => Ok(value))
    }

    fn alternative_aliased_yaml_value(input: Node) -> YamlResult<AliasedYaml> {
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

    fn hash_key(input: Node) -> YamlResult<String> {
        Ok(input.as_str().to_string())
    }

    fn hash_element(input: Node) -> YamlResult<HashElement> {
        //println!("{:#?}", input);
        match_nodes!(input.into_children();
                [hash_key(key), block_array_aliased_yaml_value(value)] => Ok(HashElement { key, value }),
        )
    }

    fn first_hash_element(input: Node) -> YamlResult<Vec<HashData>> {
        //println!("{:#?}", input);

        match_nodes!(input.into_children();
            [hash_element(element), comment(c)] => Ok(vec![HashData::Element(element), HashData::InlineComment(c)]),
            [hash_element(element)] => Ok(vec![HashData::Element(element)]),

        )
    }

    fn hash_element_data(input: Node) -> YamlResult<Vec<HashData>> {
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

    fn hash(input: Node) -> YamlResult<Yaml> {
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

    fn alternative_hash(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [first_hash_element(element1), first_hash_element(element2), hash_element_data(elements).., commentnls(cs)] => Ok(
                Yaml::Hash(
                    element1.into_iter()
                    .chain(element2.into_iter())
                    .chain(elements.into_iter().flatten())
                    .chain(cs.into_iter().map(|c| HashData::Comment(c)))
                    .collect()
                )
            ),
        )
    }

    fn block_array_element(input: Node) -> YamlResult<ArrayData> {
        match_nodes!(input.into_children();
            [block_array_aliased_yaml_value(value)] => Ok(ArrayData::Element(value)),
        )
    }

    fn first_block_array_element(input: Node) -> YamlResult<Vec<ArrayData>> {
        match_nodes!(input.into_children();
            [block_array_element(element), comment(c)] => Ok(vec![element, ArrayData::InlineComment(c)]),
            [block_array_element(element)] => Ok(vec![element]),
        )
    }

    fn block_array_data(input: Node) -> YamlResult<Vec<ArrayData>> {
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

    fn array(input: Node) -> YamlResult<Yaml> {
        match_nodes!(input.into_children();
            [inline_array(val)] => Ok(val),
            [block_array(val)] => Ok(val)
        )
    }

    fn block_array(input: Node) -> YamlResult<Yaml> {
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
        
        fn alternative_array(input: Node) -> YamlResult<Yaml> {
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

        fn commentnls(input: Node) -> YamlResult<Vec<String>> {
            //println!("{:?}", input);
            match_nodes!(
                input.into_children();
                [commentnl(cs)..] => Ok(
                    cs.collect()
                   
                )
            )
        }

        fn document(input: Node) -> YamlResult<Yaml> {
            match_nodes!(input.into_children();
        [hash(h)] => Ok(h),
        [array(a)] => Ok(a))
            
        }

        fn yaml(input: Node) -> YamlResult<Document> {
            //println!("{:#?}"  , input);
            match_nodes!(input.into_children();
                [commentnls(cs), document(val), comment(cs2).., EOI(_)] => Ok(
                    Document {
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
    YamlParser::yaml(input)
}
