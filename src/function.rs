use crate::expression::Path;
use crate::{
    Attribute, ElementList, Expression, GenericParam, NodeToElement, Param, Statement,
    SyntaxElementTrait, TypedSyntaxElement, Visibility,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode};

pub type Function<'a> = TypedSyntaxElement<'a, ast::FunctionWithBody>;
pub type FunctionDeclaration<'a> = TypedSyntaxElement<'a, ast::FunctionDeclaration>;
pub type FunctionSignature<'a> = TypedSyntaxElement<'a, ast::FunctionSignature>;

impl ElementList for ast::ImplicitsList {
    const STEP: usize = 2;
    type TSN = ast::ExprPath;
}

impl<'a> NodeToElement<'a, ast::ReturnTypeClause> for Expression<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'_, ast::ReturnTypeClause>::child_node_to_element::<
            { ast::ReturnTypeClause::INDEX_TY },
        >(db, node)
    }
}

impl<'a> NodeToElement<'a, ast::OptionReturnTypeClause> for Option<Expression<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        match node.kind(db) {
            SyntaxKind::OptionReturnTypeClauseEmpty => None,
            SyntaxKind::ReturnTypeClause => Some(
                NodeToElement::<'_, ast::ReturnTypeClause>::node_to_element(db, node),
            ),
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                node.kind(db),
                "OptionReturnTypeClause"
            ),
        }
    }
}

impl<'a> NodeToElement<'a, ast::OptionImplicitsClause> for Vec<Path<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        match node.kind(db) {
            SyntaxKind::OptionImplicitsClauseEmpty => vec![],
            SyntaxKind::ImplicitsClause => {
                NodeToElement::<'_, ast::ImplicitsList>::node_to_element(db, node)
            }
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                node.kind(db),
                "OptionImplicitsClause"
            ),
        }
    }
}

impl NodeToElement<'_, ast::FunctionDeclaration> for Vec<GenericParam<'_>> {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'_, ast::FunctionDeclaration>::child_node_to_element::<
            { ast::FunctionDeclaration::INDEX_GENERIC_PARAMS },
        >(db, node)
    }
}

impl NodeToElement<'_, ast::FunctionDeclaration> for FunctionSignature<'_> {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'_, ast::FunctionDeclaration>::child_node_to_element::<
            { ast::FunctionDeclaration::INDEX_SIGNATURE },
        >(db, node)
    }
}

impl<'a> NodeToElement<'a, ast::ExprBlock> for Vec<Statement<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'a, ast::StatementList>::child_node_to_element::<
            { ast::ExprBlock::INDEX_STATEMENTS },
        >(db, node)
    }
}

impl<'a> NodeToElement<'a, ast::FunctionWithBody> for Vec<Statement<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'a, ast::ExprBlock>::child_node_to_element::<
            { ast::FunctionWithBody::INDEX_BODY },
        >(db, node)
    }
}

impl NodeToElement<'_, ast::OptionTerminalNoPanic> for bool {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::OptionTerminalNoPanicEmpty => false,
            SyntaxKind::TerminalNoPanic => true,
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "OptionTerminalNoPanic"
            ),
        }
    }
}

impl FunctionDeclaration<'_> {
    pub const INDEX_FUNCTION_KW: usize = ast::FunctionDeclaration::INDEX_FUNCTION_KW;
    pub const INDEX_NAME: usize = ast::FunctionDeclaration::INDEX_NAME;
    pub const INDEX_GENERIC_PARAMS: usize = ast::FunctionDeclaration::INDEX_GENERIC_PARAMS;
    pub const INDEX_SIGNATURE: usize = ast::FunctionDeclaration::INDEX_SIGNATURE;
    pub fn name(&self) -> String {
        self.get_child_element::<{ ast::FunctionDeclaration::INDEX_NAME }, ast::TerminalIdentifier, String>()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        self.get_child_element::<
            { ast::FunctionDeclaration::INDEX_GENERIC_PARAMS },
            ast::OptionWrappedGenericParamList,
            Vec<GenericParam>
        >()
    }
    pub fn signature(&self) -> FunctionSignature {
        self.get_child_element::<
            { ast::FunctionDeclaration::INDEX_SIGNATURE },
            ast::FunctionSignature,
            FunctionSignature
        >()
    }
}

impl NodeToElement<'_, ast::FunctionSignature> for Vec<Param<'_>> {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'_, ast::FunctionSignature>::child_node_to_element::<
            { ast::FunctionSignature::INDEX_PARAMETERS },
        >(db, node)
    }
}

impl FunctionSignature<'_> {
    pub const INDEX_LPAREN: usize = ast::FunctionSignature::INDEX_LPAREN;
    pub const INDEX_PARAMETERS: usize = ast::FunctionSignature::INDEX_PARAMETERS;
    pub const INDEX_RPAREN: usize = ast::FunctionSignature::INDEX_RPAREN;
    pub const INDEX_RET_TY: usize = ast::FunctionSignature::INDEX_RET_TY;
    pub const INDEX_IMPLICITS_CLAUSE: usize = ast::FunctionSignature::INDEX_IMPLICITS_CLAUSE;
    pub const INDEX_OPTIONAL_NO_PANIC: usize = ast::FunctionSignature::INDEX_OPTIONAL_NO_PANIC;
    pub fn parameters(&self) -> Vec<Param> {
        self.get_child_element::<
            { ast::FunctionSignature::INDEX_PARAMETERS },
            ast::ParamList,
            Vec<Param>
        >()
    }
    pub fn ret_ty(&self) -> Option<Expression> {
        self.get_child_element::<
            { ast::FunctionSignature::INDEX_RET_TY },
            ast::OptionReturnTypeClause,
            Option<Expression>
        >()
    }
    pub fn implicits_clause(&self) -> Vec<Path> {
        self.get_child_element::<
            { ast::FunctionSignature::INDEX_IMPLICITS_CLAUSE },
            ast::OptionImplicitsClause,
            Vec<Path>
        >()
    }
    pub fn no_panic(&self) -> bool {
        let kind = self.get_child_kind::<{ FunctionSignature::INDEX_OPTIONAL_NO_PANIC }>();
        match kind {
            SyntaxKind::OptionTerminalNoPanicEmpty => false,
            SyntaxKind::TerminalNoPanic => true,
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "OptionTerminalNoPanic"
            ),
        }
    }
}

impl Function<'_> {
    pub const INDEX_ATTRIBUTES: usize = ast::FunctionWithBody::INDEX_ATTRIBUTES;
    pub const INDEX_VISIBILITY: usize = ast::FunctionWithBody::INDEX_VISIBILITY;
    pub const INDEX_DECLARATION: usize = ast::FunctionWithBody::INDEX_DECLARATION;
    pub const INDEX_BODY: usize = ast::FunctionWithBody::INDEX_BODY;
    pub fn attributes(&self) -> Vec<Attribute> {
        self.get_child_element::<
            { Function::INDEX_ATTRIBUTES },
            ast::AttributeList,
            Vec<Attribute>
        >()
    }
    pub fn visibility(&self) -> Visibility {
        self.get_child_element::<{ Function::INDEX_VISIBILITY }, ast::Visibility, Visibility>()
    }
    pub fn declaration(&self) -> FunctionDeclaration {
        self.get_child_element::<
            { Function::INDEX_DECLARATION },
            ast::FunctionDeclaration,
            FunctionDeclaration
        >()
    }
    pub fn name(&self) -> String {
        self.declaration().name()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        self.get_child_element::<
            { Function::INDEX_DECLARATION },
            ast::FunctionDeclaration,
            Vec<GenericParam>
        >()
    }
    pub fn signature(&self) -> FunctionSignature {
        self.get_child_syntax_element::<{ Function::INDEX_DECLARATION }>()
            .get_child_element::<{ FunctionDeclaration::INDEX_SIGNATURE }, ast::FunctionSignature, _>()
    }
    pub fn parameters(&self) -> Vec<Param> {
        self.get_child_syntax_element::<{ Function::INDEX_DECLARATION }>()
            .get_child_syntax_element::<{ FunctionDeclaration::INDEX_SIGNATURE }>()
            .get_child_element::<{ FunctionSignature::INDEX_PARAMETERS }, ast::ParamList, _>()
    }
    pub fn return_ty(&self) -> Option<Expression> {
        self.get_child_syntax_element::<{ Function::INDEX_DECLARATION }>()
            .get_child_syntax_element::<{ FunctionDeclaration::INDEX_SIGNATURE }>()
            .get_child_element::<{ FunctionSignature::INDEX_RET_TY }, ast::OptionReturnTypeClause, _>()
    }
    pub fn implicits_clause(&self) -> Vec<Path> {
        self.get_child_syntax_element::<{ Function::INDEX_DECLARATION }>()
            .get_child_syntax_element::<{ FunctionDeclaration::INDEX_SIGNATURE }>()
            .get_child_element::<{ FunctionSignature::INDEX_IMPLICITS_CLAUSE }, ast::OptionImplicitsClause, _>()
    }
    pub fn no_panic(&self) -> bool {
        self.get_child_syntax_element::<{ Function::INDEX_DECLARATION }>()
            .get_child_syntax_element::<{ FunctionDeclaration::INDEX_SIGNATURE }>()
            .get_child_element::<{ FunctionSignature::INDEX_OPTIONAL_NO_PANIC }, ast::OptionTerminalNoPanic, _>()
    }
}
