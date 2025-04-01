use cairo_lang_syntax::node::{ast, db::SyntaxGroup, kind::SyntaxKind, SyntaxNode};

use crate::{ElementList, NodeToElement, TypedSyntaxElement};

pub type Let<'a> = TypedSyntaxElement<'a, ast::StatementLet>;
pub type StatementExpr<'a> = TypedSyntaxElement<'a, ast::StatementExpr>;
pub type Continue<'a> = TypedSyntaxElement<'a, ast::StatementContinue>;
pub type Return<'a> = TypedSyntaxElement<'a, ast::StatementReturn>;
pub type Break<'a> = TypedSyntaxElement<'a, ast::StatementBreak>;
pub type Item<'a> = TypedSyntaxElement<'a, ast::StatementItem>;
pub type Missing<'a> = TypedSyntaxElement<'a, ast::StatementMissing>;

pub enum Statement<'a> {
    Let(Let<'a>),
    Expr(StatementExpr<'a>),
    Continue(Continue<'a>),
    Return(Return<'a>),
    Break(Break<'a>),
    Item(Item<'a>),
    Missing(Missing<'a>),
}

impl ElementList for ast::StatementList {
    const STEP: usize = 1;
    type TSN = ast::Statement;
}

impl<'a> NodeToElement<'a, ast::Statement> for Statement<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::StatementLet => Statement::Let(Let::node_to_element(db, node)),
            SyntaxKind::StatementExpr => Statement::Expr(StatementExpr::node_to_element(db, node)),
            SyntaxKind::StatementContinue => {
                Statement::Continue(Continue::node_to_element(db, node))
            }
            SyntaxKind::StatementReturn => Statement::Return(Return::node_to_element(db, node)),
            SyntaxKind::StatementBreak => Statement::Break(Break::node_to_element(db, node)),
            SyntaxKind::StatementItem => Statement::Item(Item::node_to_element(db, node)),
            SyntaxKind::StatementMissing => Statement::Missing(Missing::node_to_element(db, node)),
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "Statement"
            ),
        }
    }
}
