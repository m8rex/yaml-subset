pub fn indent(amount: usize) -> String {
    (0..amount).map(|_| " ").collect::<Vec<_>>().join("")
}

fn should_write_literal(s: &String) -> bool {
    s.contains("\n") || s.contains("\\") // TODO
}

fn needs_quotes(s: &String) -> bool {
    s.is_empty() || s.contains(": ") || s.contains(" #")
}

mod tests {

    #[test]
    fn indent() {
        assert_eq!(super::indent(1), " ".to_string());
        assert_eq!(super::indent(2), "  ".to_string());
        assert_eq!(super::indent(3), "   ".to_string());
        assert_eq!(super::indent(4), "    ".to_string());
        assert_eq!(super::indent(5), "     ".to_string());
        assert_eq!(super::indent(6), "      ".to_string());
        assert_eq!(super::indent(7), "       ".to_string());
        assert_eq!(super::indent(8), "        ".to_string());
    }
}
