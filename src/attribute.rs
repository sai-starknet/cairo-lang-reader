use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::helpers::QueryAttrs as NodeQueryAttrs;
use cairo_lang_syntax::node::TypedSyntaxNode;

use crate::expression::Path;
use crate::{Arg, SyntaxElementTrait, TypedSyntaxElement};

pub type Attribute<'a> = TypedSyntaxElement<'a, ast::Attribute>;

impl Attribute<'_> {
    pub const INDEX_HASH: usize = ast::Attribute::INDEX_HASH;
    pub const INDEX_LBRACK: usize = ast::Attribute::INDEX_LBRACK;
    pub const INDEX_ATTR: usize = ast::Attribute::INDEX_ATTR;
    pub const INDEX_ARGUMENTS: usize = ast::Attribute::INDEX_ARGUMENTS;
    pub const INDEX_RBRACK: usize = ast::Attribute::INDEX_RBRACK;
    pub fn attr(&self) -> Path {
        self.get_child_element::<{ Attribute::INDEX_ATTR }, ast::ExprPath, Path>()
    }
    pub fn arguments(&self) -> Vec<Arg> {
        self.get_child_element::<{Attribute::INDEX_ARGUMENTS}, ast::OptionArgListParenthesized, _>()
    }
}

/// Trait for querying attributes of AST items.
pub trait QueryAttrs<'a>
where
    Self: SyntaxElementTrait<'a>,
{
    // fn try_attributes(&self) -> Option<>;
    // /// Generic call `self.attributes(db).elements(db)`.
    // ///
    // /// Implementation detail, should not be used by this trait users.
    fn attributes_elements(&self) -> impl Iterator<Item = Attribute<'a>>;
    /// Collect all attributes named exactly `attr` attached to this node.
    fn query_attr(&self, attr: &str) -> impl Iterator<Item = Attribute<'a>> {
        self.attributes_elements()
            .filter(move |a| a.attr().get_text_without_trivia() == attr)
    }

    /// Find first attribute named exactly `attr` attached do this node.
    fn find_attr(&self, attr: &str) -> Option<Attribute<'a>> {
        self.query_attr(attr).next()
    }

    /// Check if this node has an attribute named exactly `attr`.
    fn has_attr(&self, attr: &str) -> bool {
        self.find_attr(attr).is_some()
    }

    // /// Checks if the given object has an attribute with the given name and argument.
    // fn has_attr_with_arg(&self, attr_name: &str, arg_name: &str) -> bool {
    //     self.query_attr(attr_name)
    //         .any(|attr| is_single_arg_attr(self.get_db(), &attr, arg_name))
    // }
}

impl<'a, TSN: TypedSyntaxNode> QueryAttrs<'a> for TypedSyntaxElement<'a, TSN>
where
    TSN: NodeQueryAttrs + Clone,
{
    fn attributes_elements(&self) -> impl Iterator<Item = Attribute<'a>> {
        let db = self.get_db();
        let tsn = self.as_typed_syntax_node::<TSN>().clone();
        TSN::try_attributes(&tsn, db)
            .unwrap()
            .elements(db)
            .map(move |x| Attribute::from_typed_syntax_node(db, x))
    }
}
