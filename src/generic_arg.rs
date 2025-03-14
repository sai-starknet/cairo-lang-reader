use crate::syntax_element::ToTypedSyntaxElementLike;

use crate::TypedSyntaxElement;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode};

pub type GenericArgUnnamed<'a> = TypedSyntaxElement<'a, ast::GenericArgUnnamed>;
pub type GenericArgNamed<'a> = TypedSyntaxElement<'a, ast::GenericArgNamed>;

pub enum GenericArg<'a> {
    Unnamed(GenericArgUnnamed<'a>),
    Named(GenericArgNamed<'a>),
}

impl<'a> ToTypedSyntaxElementLike<'a> for GenericArg<'a> {
    fn to_syntax_element(db: &'a dyn SyntaxGroup, syntax_node: SyntaxNode) -> GenericArg<'a> {
        let kind = syntax_node.kind(db);
        match kind {
            SyntaxKind::GenericArgUnnamed => {
                GenericArg::Unnamed(GenericArgUnnamed::from_syntax_node(db, syntax_node))
            }
            SyntaxKind::GenericArgNamed => {
                GenericArg::Named(GenericArgNamed::from_syntax_node(db, syntax_node))
            }
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "GenericArg"
            ),
        }
    }
}
