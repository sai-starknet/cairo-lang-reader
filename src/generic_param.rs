use crate::{
    ElementList, Expression, NodeToElement, PathSegment, SyntaxElementTrait, TypedSyntaxElement,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode, TypedSyntaxNode};

pub type Type<'a> = TypedSyntaxElement<'a, ast::GenericParamType>;
pub type Const<'a> = TypedSyntaxElement<'a, ast::GenericParamConst>;
pub type ImplNamed<'a> = TypedSyntaxElement<'a, ast::GenericParamImplNamed>;
pub type ImplAnonymous<'a> = TypedSyntaxElement<'a, ast::GenericParamImplAnonymous>;
pub type NegativeImpl<'a> = TypedSyntaxElement<'a, ast::GenericParamNegativeImpl>;

pub type OptionWrappedGenericParamList<'a> =
    TypedSyntaxElement<'a, ast::OptionWrappedGenericParamList>;

pub enum GenericParam<'a> {
    Type(String),
    Const(Const<'a>),
    ImplNamed(ImplNamed<'a>),
    ImplAnonymous(ImplAnonymous<'a>),
    NegativeImpl(NegativeImpl<'a>),
}

impl<'a> NodeToElement<'a, ast::GenericParam> for GenericParam<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::GenericParamType => {
                GenericParam::Type(NodeToElement::<'a, ast::GenericParamType>::node_to_element(
                    db, node,
                ))
            }
            SyntaxKind::GenericParamConst => GenericParam::Const(Const::node_to_element(db, node)),
            SyntaxKind::GenericParamImplNamed => {
                GenericParam::ImplNamed(ImplNamed::node_to_element(db, node))
            }
            SyntaxKind::GenericParamImplAnonymous => {
                GenericParam::ImplAnonymous(ImplAnonymous::node_to_element(db, node))
            }
            SyntaxKind::GenericParamNegativeImpl => {
                GenericParam::NegativeImpl(NegativeImpl::node_to_element(db, node))
            }
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "GenericParam"
            ),
        }
    }
}

impl<'a> NodeToElement<'a, ast::GenericParamType> for String {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'a, ast::TerminalIdentifier>::child_node_to_element::<
            { ast::GenericParamType::INDEX_NAME },
        >(db, node)
    }
}

impl ElementList for ast::GenericParamList {
    const STEP: usize = 2;
    type TSN = ast::GenericParam;
}

impl<'a> NodeToElement<'a, ast::WrappedGenericParamList> for Vec<GenericParam<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'a, ast::GenericParamList>::child_node_to_element::<
            { ast::WrappedGenericParamList::INDEX_GENERIC_PARAMS },
        >(db, node)
    }
}

impl<'a> NodeToElement<'a, ast::OptionWrappedGenericParamList> for Vec<GenericParam<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::OptionWrappedGenericParamListEmpty => vec![],
            SyntaxKind::WrappedGenericParamList => {
                NodeToElement::<'a, ast::WrappedGenericParamList>::node_to_element(db, node)
            }
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "OptionWrappedGenericParamList"
            ),
        }
    }
}

impl<'a> Const<'a> {
    pub const INDEX_CONST_KW: usize = ast::GenericParamConst::INDEX_CONST_KW;
    pub const INDEX_NAME: usize = ast::GenericParamConst::INDEX_NAME;
    pub const INDEX_COLON: usize = ast::GenericParamConst::INDEX_COLON;
    pub const INDEX_TY: usize = ast::GenericParamConst::INDEX_TY;

    pub fn name(&self) -> String {
        self.get_child_element::<{ Const::INDEX_NAME }, ast::TerminalIdentifier, _>()
    }
    pub fn ty(&self) -> Expression<'a> {
        self.get_child_element::<{ Const::INDEX_TY }, ast::Expr, _>()
    }
}

impl<'a> ImplNamed<'a> {
    pub const INDEX_IMPL_KW: usize = ast::GenericParamImplNamed::INDEX_IMPL_KW;
    pub const INDEX_NAME: usize = ast::GenericParamImplNamed::INDEX_NAME;
    pub const INDEX_COLON: usize = ast::GenericParamImplNamed::INDEX_COLON;
    pub const INDEX_TRAIT_PATH: usize = ast::GenericParamImplNamed::INDEX_TRAIT_PATH;
    pub const INDEX_TYPE_CONSTRAINS: usize = ast::GenericParamImplNamed::INDEX_TYPE_CONSTRAINS;

    pub fn name(&self) -> String {
        self.get_child_element::<{ ImplNamed::INDEX_NAME }, ast::TerminalIdentifier, _>()
    }
    pub fn trait_path(&self) -> Vec<PathSegment<'a>> {
        self.get_child_element::<{ ImplNamed::INDEX_TRAIT_PATH }, ast::ExprPath, _>()
    }
}
