use crate::{Expression, NodeToElement, SyntaxElementTrait, TypedSyntaxElement};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode};

pub type GenericArgUnnamed<'a> = TypedSyntaxElement<'a, ast::GenericArgUnnamed>;
pub type GenericArgNamed<'a> = TypedSyntaxElement<'a, ast::GenericArgNamed>;

pub enum GenericArg<'a> {
    Unnamed(Expression<'a>),
    Named(GenericArgNamed<'a>),
}

impl<'a> NodeToElement<'a, ast::GenericArgValueExpr> for Expression<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<ast::Expr>::child_node_to_element::<{ ast::GenericArgValueExpr::INDEX_EXPR }>(
            db, node,
        )
    }
}

impl<'a> NodeToElement<'a, ast::GenericArgValue> for Expression<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::GenericArgValueExpr => {
                NodeToElement::<'a, ast::GenericArgValueExpr>::node_to_element(db, node)
            }
            SyntaxKind::TerminalUnderscore => Expression::Underscore,
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "GenericArgValue"
            ),
        }
    }
}

impl<'a> NodeToElement<'a, ast::GenericArgUnnamed> for Expression<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'a, ast::GenericArgValue>::child_node_to_element::<
            { ast::GenericArgUnnamed::INDEX_VALUE },
        >(db, node)
    }
}

impl<'a> NodeToElement<'a, ast::GenericArgNamed> for Expression<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'a, ast::GenericArgValue>::child_node_to_element::<
            { ast::GenericArgNamed::INDEX_VALUE },
        >(db, node)
    }
}

impl<'a> NodeToElement<'a, ast::GenericArg> for GenericArg<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::GenericArgUnnamed => {
                GenericArg::Unnamed(
                    NodeToElement::<'_, ast::GenericArgUnnamed>::node_to_element(db, node),
                )
            }
            SyntaxKind::GenericArgNamed => {
                GenericArg::Named(NodeToElement::<'a, ast::GenericArgNamed>::node_to_element(
                    db, node,
                ))
            }
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "GenericArg"
            ),
        }
    }
}

impl<'a> GenericArgNamed<'a> {
    pub const INDEX_NAME: usize = ast::GenericArgNamed::INDEX_NAME;
    pub const INDEX_COLON: usize = ast::GenericArgNamed::INDEX_COLON;
    pub const INDEX_VALUE: usize = ast::GenericArgNamed::INDEX_VALUE;
    pub fn name(&self) -> String {
        self.get_child_element::<{ GenericArgNamed::INDEX_NAME }, ast::TerminalIdentifier, _>()
    }
    pub fn value(&self) -> Expression<'a> {
        self.get_child_element::<{ GenericArgNamed::INDEX_VALUE }, ast::GenericArgValue, _>()
    }
}

impl<'a> GenericArg<'a> {
    fn name(&self) -> Option<String> {
        match self {
            GenericArg::Unnamed(_) => None,
            GenericArg::Named(named) => Some(named.name()),
        }
    }

    fn value(self) -> Expression<'a> {
        match self {
            GenericArg::Unnamed(unnamed) => unnamed,
            GenericArg::Named(named) => named.value(),
        }
    }
}
