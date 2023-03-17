extern crate pest;

mod path;
mod utils;
pub mod yaml;

pub use path::YamlPath;
pub use yaml::{DocumentResult, YamlResult};

#[cfg(test)]
mod tests {
    use super::yaml::parse_yaml_file;
    use super::yaml::{AliasedYaml, Yaml, YamlInsert};
    use super::YamlPath;
    use crate::yaml::{DoubleQuotedStringPart, MyYamlVec};

    #[test]
    fn basic() {
        let inp = r#"---
# test
k: &anch test
# hi
v: &ok
   - &ok2 test
# comment
   - *test
v: test # maybe
k: [5, 10, "e"]
variable_groups: []
double_quoted: "test

now a space \n vs \\n
"
single_quoted: 'test escaping ''

now a space \n vs \\n
'
l:
 - 'sqrt( {d}^({r*n+m}m+{p}) * x^({r*a+p})  * y^{b*r+q} )'
 - Read the values of the cyclometric numbers off of the circle. <br/> {gon_circle}

# after
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn inline_hash() {
        let inp = r#"---
field: &test {}
item: {}
other: # check
  item: {}
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn block_delimiters_in_text() {
        let inp = r#"---
content: When you press <em>Submit part</em>.
abs: An absolute value is written as |x|
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn array_delimiters_in_text() {
        let inp = r#"---
default_value:  random(-9 .. 9 except [0, a])    
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn comments_before_start() {
        let inp = r#"
# a comment
# another one

---
x: 5

  "#;

        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn elab() {
        let inp = r#"---
# test
k: &anch test
# hi

v: &ok
   - &ok2 test
# comment
   - *test # an anchor
v: test # maybe
k: &kk [5, 10, "e"]
# after
y: - x: 5
     z: &zfield 6 # yess

   - n: 8

   - j
# between
z: - *kk

   - ke
k: - - j
     - k

     - l
   - - m
     - z: k


       t: - m: l # comment
            p: 4 # comment
          - m # after m
# wow
# something else
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn block_scalars() {
        // see https://yaml-multiline.info/
        let inp = r#"---
        newlines: &newline |
                  Several lines of text,
                  with some "quotes" of various 'types',
                  and also a blank line:
                  
                  and some text with
                    extra indentation
                  on the next line,
                  plus another line at the end.
                  
        folded: &folded >
                  Several lines of text,
                  with some "quotes" of various 'types',
                  and also a blank line:
                  
                  and some text with
                    extra indentation
                  on the next line,
                  plus another line at the end.
                  
                  
                  
        test: 5"#;

        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn block_scalars_deeper() {
        let inp = r#"---
parts:
  - 
    type: gapfill
    marks: 0
    prompt:
      content: test
      other: |
        8
        10"#;

        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn commas_in_unquoted_string() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
"#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn alternative_hash_comment_between_first_and_second() {
        let inp = r#"---
parts:
- type: choose_one
  # prompt: "file:statement-wrong.html"
  answer_data: tmp 
  "#;

        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn comment_before_first_key() {
        let inp = r#"---
default_value:
    content: 
      # NOTE: the 'original' non-shuffled array advr is used, rather than x_advr !!!
      # Better advice to be written ...? This one does not correctly extend to users of this template!!
      nl: "Gebruik het merkwaardig product {if(a*b>0, advr[0], advr[1])}"
      en: "Use the special product {if(a*b>0, advr[0], advr[1])}""#;
        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }

    #[test]
    fn insert_hash_top() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "new".parse().unwrap();
        let data = AliasedYaml {
            alias: None,
            value: Yaml::UnquotedString("value".to_string()),
        };
        assert_eq!(1, parsed.insert_into_hash(&path, &data, false));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
new: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }

    #[test]
    fn insert_hash() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key2".parse().unwrap();
        let data = AliasedYaml {
            alias: None,
            value: Yaml::DoubleQuotedString(vec![DoubleQuotedStringPart::String(
                "test".to_string(),
            )]),
        };
        assert_eq!(1, parsed.insert_into_hash(&path, &data, false));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
 key2: "test"
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);

        let path: YamlPath = "item.key2".parse().unwrap();
        let data = AliasedYaml {
            alias: None,
            value: Yaml::DoubleQuotedString(vec![DoubleQuotedStringPart::String(
                "test2".to_string(),
            )]),
        };
        assert_eq!(0, parsed.insert_into_hash(&path, &data, false));
        assert_eq!(parsed_out, parsed);
        assert_eq!(1, parsed.insert_into_hash(&path, &data, true));
        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
 key2: "test2"
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }

    #[test]
    fn rename_field() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key".parse().unwrap();
        assert_eq!(1, parsed.rename_field(&path, "newkey".to_string()));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 newkey: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }

    #[test]
    fn rename_field_list_in_list() {
        let inp = r#"---
parts:
  - type: gapfill
    gaps:
      - type: jme
        old_name: value
      - type: jme
        old_name: value
  - type: random 
    gaps:
      - type: jme
        old_name: value
      - type: jme
        old_name: value
  - type: gapfill
    gaps:
      - type: jme
        old_name: value
      - type: jme
        old_name: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "parts[*]|type=gapfill.gaps[*].old_name".parse().unwrap();
        assert_eq!(4, parsed.rename_field(&path, "new_name".to_string()));

        let out = r#"---
parts:
  - type: gapfill
    gaps:
      - type: jme
        new_name: value
      - type: jme
        new_name: value
  - type: random 
    gaps:
      - type: jme
        old_name: value
      - type: jme
        old_name: value
  - type: gapfill
    gaps:
      - type: jme
        new_name: value
      - type: jme
        new_name: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }

    #[test]
    fn to_object() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key".parse().unwrap();
        assert_eq!(1, parsed.to_object(&path, "subkey".to_string()));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: 
   subkey: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn to_object_with_condition() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key|key=value".parse().unwrap();
        assert_eq!(1, parsed.to_object(&path, "subkey".to_string()));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: 
   subkey: value
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key|key=other".parse().unwrap();
        assert_eq!(0, parsed.to_object(&path, "subkey".to_string()));
        assert_eq!(parse_yaml_file(inp).unwrap(), parsed);
    }
    #[test]
    fn remove_from_hash() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key: value
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item".parse().unwrap();
        assert_eq!(1, parsed.remove_from_hash(&path));

        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn remove_from_hash_deeper() {
        let inp = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key:
  item: 5
  item2: 6
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item.key.item".parse().unwrap();
        assert_eq!(1, parsed.remove_from_hash(&path));
        let out = r#"---
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
  key:
    item2: 6
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn remove_from_hash_deeper_with_condition() {
        let inp = r#"---
type: test
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
 key:
  item: 5
  item2: 6
"#;
        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item|type=test.key.item".parse().unwrap();
        assert_eq!(1, parsed.remove_from_hash(&path));
        let out = r#"---
type: test
inline_array: [test, 5, hi]
s: &key test, 5, hi
item:
  key:
    item2: 6
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "item|type=other.key.item".parse().unwrap();
        assert_eq!(0, parsed.remove_from_hash(&path));
        assert_eq!(parse_yaml_file(inp).unwrap(), parsed);
    }
    #[test]
    fn move_to_subfield_in_root() {
        let inp = r#"---
a: vala
b: valb
c: valc
d: vald
e: vale
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(
                &path,
                "sub".to_string(),
                vec!["e".to_string(), "b".to_string(), "d".to_string()]
            )
        );
        let out = r#"---
a: vala
c: valc
sub:
    e: vale
    b: valb
    d: vald
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_subfield() {
        let inp = r#"---
k:
  a: vala
  b: valb
  c: valc
  d: vald
  e: vale
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(
                &path,
                "sub".to_string(),
                vec!["e".to_string(), "b".to_string(), "d".to_string()]
            )
        );
        let out = r#"---
k:
  a: vala
  c: valc
  sub:
    e: vale
    b: valb
    d: vald
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_existing_subfield() {
        let inp = r#"---
k:
  a: vala
  b: valb
  c: valc
  d: vald
  e: vale
  l:
    z: valz
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(
                &path,
                "l".to_string(),
                vec![
                    "e".to_string(),
                    "b".to_string(),
                    "d".to_string(),
                    "kk".to_string()
                ]
            )
        );
        let out = r#"---
k:
  a: vala
  c: valc
  l:
    z: valz
    e: vale
    b: valb
    d: vald
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_subfield_list() {
        let inp = r#"---
k:
  - item1: test0
    result:
      yes: three
  - item1: test1
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k[*]".parse().unwrap();
        assert_eq!(
            2,
            parsed.move_to_subfield(&path, "result".to_string(), vec!["item1".to_string(),])
        );
        let out = r#"---
k:
  - result:
      yes: three
      item1: test0
  - result:
      item1: test1
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_subfield_list_item() {
        let inp = r#"---
k:
  - item1: test0
    result:
      yes: three
  - item1: test1
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k[0]".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(&path, "result".to_string(), vec!["item1".to_string(),])
        );
        let out = r#"---
k:
  - result:
      yes: three
      item1: test0
  - item1: test1
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn move_to_subfield_list_filter() {
        let inp = r#"---
k:
  - item1: test0
    result:
      yes: three
  - item1: test1
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k[*]|item1=test1".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_subfield(&path, "result".to_string(), vec!["item1".to_string(),])
        );
        let out = r#"---
k:
  - item1: test0
    result:
      yes: three      
  - result:
      item1: test1
"#;

        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }
    #[test]
    fn neg_number_in_list() {
        let inp = r#"---
vset_range:
  - -10
  - 10
vset_range_points: 20
"#;

        let parsed = parse_yaml_file(inp);
        insta::assert_debug_snapshot!(parsed);
        insta::assert_display_snapshot!(parsed.unwrap().format().unwrap());
    }
    #[test]
    fn move_to_map_with_field_as_key() {
        let inp = r#"---
k:
    - item1: 
        variables:
            # this specifies the test
            test: value
            ke:
              # comments
              definition: result
              group: keep
            # this one will be moved
            other:
              definition: result
              group: different
            k: 
              definition: value
              group: ""
            main:
              definiton: field
              group: other
            other2:
              definition: result2
              group: different
    - item2:
        variables:
            # this specifies the test
            test: value
        grouped_variables: anything
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k[*].item1".parse().unwrap();
        assert_eq!(
            1,
            parsed.move_to_map_with_field_as_key(
                &path,
                "variables".to_string(),
                "group".to_string(),
                "grouped_variables".to_string(),
                vec!["keep".to_string()]
            )
        );
        let out = r#"---
k:
- item1: 
        variables:
            # this specifies the test
            test: value
            ke:
              # comments
              definition: result
            k: 
              definition: value
        grouped_variables:
            different:
                # this one will be moved
                other:
                    definition: result
                other2:
                    definition: result2
            other:
                main:
                    definiton: field
- item2:
    variables:
        # this specifies the test
        test: value
    grouped_variables: anything
"#;
        println!("{}", parsed.format().unwrap());
        let parsed_out = parse_yaml_file(out).unwrap();
        assert_eq!(parsed_out, parsed);
    }

    #[test]
    fn find() {
        let inp = r#"---
k:
  - item1: test0
    result:
      yes: three
  - item1: test1
"#;

        let mut parsed = parse_yaml_file(inp).unwrap();
        let path: YamlPath = "k[*].item1".parse().unwrap();
        assert_eq!(
            MyYamlVec(vec![
                Yaml::UnquotedString("test0".to_string()),
                Yaml::UnquotedString("test1".to_string()),
            ]),
            parsed.find(&path)
        );
    }
}
