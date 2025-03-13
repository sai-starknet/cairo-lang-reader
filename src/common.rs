use super::{DbSyntaxNode, DbTns, NewDbTypedSyntaxNode};
use crate::element_list_to_vec;
use crate::expression;
use crate::Expression;
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::Terminal;

pub type Attribute<'a> = DbTns<'a, ast::Attribute>;
pub type Arg<'a> = DbTns<'a, ast::Arg>;
pub type Param<'a> = DbTns<'a, ast::Param>;

#[derive(Clone, Debug)]
pub enum Visibility {
    Default,
    Pub,
}

pub enum Modifier {
    Ref,
    Mut,
}

impl Attribute<'_> {
    pub fn attr(&self) -> expression::Path {
        expression::Path::new(self.db(), self.tsn.attr(self.db()))
    }
    pub fn arguments(&self) -> Vec<Arg> {
        option_args_parenthesized_to_vec(self.db(), self.tsn.arguments(self.db()))
    }
}

impl Param<'_> {
    pub fn modifiers(&self) -> Vec<Modifier> {
        ast_modifiers_to_modifier(self.db(), self.tsn.modifiers(self.db()))
    }
    pub fn name(&self) -> String {
        self.tsn.name(self.db()).text(self.db()).to_string()
    }
    pub fn ty(&self) -> Option<Expression> {
        match self.tsn.type_clause(self.db()) {
            ast::OptionTypeClause::Empty(_) => None,
            ast::OptionTypeClause::TypeClause(tsn) => {
                Some(Expression::new(self.db(), tsn.ty(self.db())))
            }
        }
    }
}

pub fn option_args_parenthesized_to_vec<'a>(
    db: &'a dyn SyntaxGroup,
    option_args_parenthesized: ast::OptionArgListParenthesized,
) -> Vec<Arg<'a>> {
    match option_args_parenthesized {
        ast::OptionArgListParenthesized::Empty(_) => vec![],
        ast::OptionArgListParenthesized::ArgListParenthesized(tsn) => {
            element_list_to_vec(db, tsn.arguments(db))
        }
    }
}

pub fn ast_modifiers_to_modifier<'a>(
    db: &'a dyn SyntaxGroup,
    modifier_list: ast::ModifierList,
) -> Vec<Modifier> {
    modifier_list
        .elements(db)
        .iter()
        .map(|m| match m {
            ast::Modifier::Ref(_) => Modifier::Ref,
            ast::Modifier::Mut(_) => Modifier::Mut,
        })
        .collect()
}
