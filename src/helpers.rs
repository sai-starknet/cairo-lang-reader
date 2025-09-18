use crate::Attribute;

pub fn is_single_arg_attr(attr: &Attribute, arg_name: &str) -> bool {
    match attr.arguments() {
        OptionArgListParenthesized::ArgListParenthesized(args) => {
            matches!(&args.arguments(db).elements_vec(db)[..],
                    [arg] if arg.as_syntax_node().get_text_without_trivia(db) == arg_name)
        }
        OptionArgListParenthesized::Empty(_) => false,
    }
}
