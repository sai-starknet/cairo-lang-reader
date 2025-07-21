use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::helpers::QueryAttrs as NodeQueryAttrs;
use cairo_lang_syntax::node::{helpers::is_single_arg_attr, TypedSyntaxNode};

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
    /// Generic call `self.attributes(db).elements(db)`.
    ///
    /// Implementation detail, should not be used by this trait users.
    #[doc(hidden)]
    fn attributes_elements(&self) -> Vec<ast::Attribute>;
    fn query_attr_node(&self, attr: &str) -> Vec<ast::Attribute> {
        let db = self.get_db();
        self.attributes_elements()
            .into_iter()
            .filter(|a| a.attr(db).as_syntax_node().get_text_without_trivia(db) == attr)
            .collect()
    }
    /// Collect all attributes named exactly `attr` attached to this node.
    fn query_attr(&self, attr: &str) -> Vec<Attribute<'a>> {
        let db = self.get_db();
        self.attributes_elements()
            .into_iter()
            .filter(|a| a.attr(db).as_syntax_node().get_text_without_trivia(db) == attr)
            .map(|a| Attribute::from_typed_syntax_node(db, a))
            .collect()
    }

    /// Find first attribute named exactly `attr` attached do this node.
    fn find_attr(&self, attr: &str) -> Option<Attribute<'a>> {
        let db = self.get_db();
        for a in self.attributes_elements().into_iter() {
            if a.attr(db).as_syntax_node().get_text_without_trivia(db) == attr {
                return Some(Attribute::from_typed_syntax_node(db, a));
            }
        }
        None
    }

    /// Check if this node has an attribute named exactly `attr`.
    fn has_attr(&self, attr: &str) -> bool {
        self.find_attr(attr).is_some()
    }

    /// Checks if the given object has an attribute with the given name and argument.
    fn has_attr_with_arg(&self, attr_name: &str, arg_name: &str) -> bool {
        self.query_attr_node(attr_name)
            .iter()
            .any(|attr| is_single_arg_attr(self.get_db(), attr, arg_name))
    }
}

impl<'a, TSN: TypedSyntaxNode> QueryAttrs<'a> for TypedSyntaxElement<'a, TSN>
where
    TSN: NodeQueryAttrs,
{
    fn attributes_elements(&self) -> Vec<ast::Attribute> {
        TSN::attributes_elements(&self.as_typed_syntax_node(), self.get_db())
    }
}
