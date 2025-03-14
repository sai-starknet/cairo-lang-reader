use crate::expression::Path;
use crate::generic_param::option_wrapped_generic_params_to_vec;
use crate::{
    element_list_to_vec, Attribute, DbSyntaxNode, DbTns, Expression, GenericParam,
    NewDbTypedSyntaxNode, Param, Visibility,
};
use cairo_lang_syntax::node::ast;

pub type Function<'a> = DbTns<'a, ast::FunctionWithBody>;
pub type FunctionDeclaration<'a> = DbTns<'a, ast::FunctionDeclaration>;
pub type FunctionSignature<'a> = DbTns<'a, ast::FunctionSignature>;

impl FunctionDeclaration<'_> {
    pub fn name(&self) -> String {
        self.tsn.name(self.db()).text(self.db()).to_string()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        option_wrapped_generic_params_to_vec(self.db(), self.tsn.generic_params(self.db()))
    }
    pub fn signature(&self) -> FunctionSignature {
        FunctionSignature::new(self.db(), self.tsn.signature(self.db()))
    }
}

impl FunctionSignature<'_> {
    pub fn parameters(&self) -> Vec<Param> {
        element_list_to_vec(self.db(), self.tsn.parameters(self.db()))
    }
    pub fn return_ty(&self) -> Option<Expression> {
        match self.tsn.ret_ty(self.db()) {
            ast::OptionReturnTypeClause::Empty(_) => None,
            ast::OptionReturnTypeClause::ReturnTypeClause(tsn) => {
                Some(Expression::new(self.db(), tsn.ty(self.db())))
            }
        }
    }
    pub fn implicits_clause(&self) -> Vec<Path> {
        match self.tsn.implicits_clause(self.db()) {
            ast::OptionImplicitsClause::Empty(_) => vec![],
            ast::OptionImplicitsClause::ImplicitsClause(tsn) => {
                element_list_to_vec(self.db(), tsn.implicits(self.db()))
            }
        }
    }
    pub fn no_panic(&self) -> bool {
        match self.tsn.optional_no_panic(self.db()) {
            ast::OptionTerminalNoPanic::Empty(_) => false,
            ast::OptionTerminalNoPanic::TerminalNoPanic(_) => true,
        }
    }
}

impl Function<'_> {
    pub fn attributes(&self) -> Vec<Attribute> {
        element_list_to_vec(self.db(), self.tsn.attributes(self.db()))
    }
    pub fn visibility(&self) -> Visibility {
        match self.tsn.visibility(self.db()) {
            ast::Visibility::Pub(_) => Visibility::Pub,
            ast::Visibility::Default(_) => Visibility::Default,
        }
    }
    pub fn ast_declaration(&self) -> ast::FunctionDeclaration {
        self.tsn.declaration(self.db())
    }
    pub fn declaration(&self) -> FunctionDeclaration {
        FunctionDeclaration::new(self.db(), self.ast_declaration())
    }
    pub fn name(&self) -> String {
        self.declaration().name()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        option_wrapped_generic_params_to_vec(
            self.db(),
            self.ast_declaration().generic_params(self.db()),
        )
    }
    pub fn ast_signature(&self) -> ast::FunctionSignature {
        self.ast_declaration().signature(self.db())
    }
    pub fn signature(&self) -> FunctionSignature {
        FunctionSignature::new(self.db(), self.ast_signature())
    }
    pub fn parameters(&self) -> Vec<Param> {
        element_list_to_vec(self.db(), self.ast_signature().parameters(self.db()))
    }
    pub fn return_ty(&self) -> Option<Expression> {
        match self.ast_signature().ret_ty(self.db()) {
            ast::OptionReturnTypeClause::Empty(_) => None,
            ast::OptionReturnTypeClause::ReturnTypeClause(tsn) => {
                Some(Expression::new(self.db(), tsn.ty(self.db())))
            }
        }
    }
    pub fn implicits_clause(&self) -> Vec<Path> {
        match self.ast_signature().implicits_clause(self.db()) {
            ast::OptionImplicitsClause::Empty(_) => vec![],
            ast::OptionImplicitsClause::ImplicitsClause(tsn) => {
                element_list_to_vec(self.db(), tsn.implicits(self.db()))
            }
        }
    }
    pub fn no_panic(&self) -> bool {
        match self.ast_signature().optional_no_panic(self.db()) {
            ast::OptionTerminalNoPanic::Empty(_) => false,
            ast::OptionTerminalNoPanic::TerminalNoPanic(_) => true,
        }
    }
}
