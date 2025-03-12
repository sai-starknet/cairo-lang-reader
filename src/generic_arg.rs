use super::{DbSyntaxNode, DbTns, DbTypedSyntaxNode, DynDbSyntaxNode, NewDbTypedSyntaxNode};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

pub type GenericArgUnnamed<'a> = DbTns<'a, ast::GenericArgUnnamed>;
pub type GenericArgNamed<'a> = DbTns<'a, ast::GenericArgNamed>;

pub enum GenericArg<'a> {
    Unnamed(GenericArgUnnamed<'a>),
    Named(GenericArgNamed<'a>),
}

impl<'a> DynDbSyntaxNode<'a> for GenericArg<'a> {
    fn to_dyn_db_ast_trait(&self) -> &dyn DbSyntaxNode {
        match self {
            GenericArg::Unnamed(expr) => expr,
            GenericArg::Named(expr) => expr,
        }
    }
}

impl<'a> NewDbTypedSyntaxNode<'a> for GenericArg<'a> {
    type TSN = ast::GenericArg;
    fn new(db: &'a dyn SyntaxGroup, node: ast::GenericArg) -> GenericArg<'a> {
        match node {
            ast::GenericArg::Unnamed(expr) => GenericArg::Unnamed(GenericArgUnnamed::new(db, expr)),
            ast::GenericArg::Named(expr) => GenericArg::Named(GenericArgNamed::new(db, expr)),
        }
    }
}

impl<'a> DbTypedSyntaxNode<'a> for GenericArg<'a> {
    type TSN = ast::GenericArg;
    fn typed_syntax_node(&self) -> Self::TSN {
        ast::GenericArg::from_syntax_node(self.db(), self.syntax_node())
    }
}
