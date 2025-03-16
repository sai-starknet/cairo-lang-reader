use crate::{Expression, NodeToElement, TypedSyntaxElement};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode};

pub type GenericArgUnnamed<'a> = TypedSyntaxElement<'a, ast::GenericArgUnnamed>;
pub type GenericArgNamed<'a> = TypedSyntaxElement<'a, ast::GenericArgNamed>;

pub enum GenericArg<'a> {
    Unnamed(GenericArgValue<'a>),
    Named(GenericArgNamed<'a>),
}

pub enum GenericArgValue<'a> {
    Expr(Expression<'a>),
    Underscore,
}

impl NodeToElement<'_, ast::GenericArg> for GenericArg<'_> {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> GenericArg {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::GenericArgUnnamed => {
                GenericArg::Unnamed(
                    NodeToElement::<'_, ast::GenericArgValue>::child_node_to_element::<0>(db, node),
                )
            }
            SyntaxKind::GenericArgNamed => {
                GenericArg::Named(GenericArgNamed::from_syntax_node(db, node))
            }
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "GenericArg"
            ),
        }
    }
}

impl NodeToElement<'_, ast::GenericArgValue> for GenericArgValue<'_> {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> GenericArgValue {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::GenericArgValueExpr => GenericArgValue::Expr(
                NodeToElement::<'_, ast::Expr>::child_node_to_element::<0>(db, node),
            ),
            SyntaxKind::TerminalUnderscore => GenericArgValue::Underscore,
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "GenericArgValue"
            ),
        }
    }
}
