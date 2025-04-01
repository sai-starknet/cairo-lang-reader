use crate::{ElementList, NodeToElement, TypedSyntaxElement};
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
    Type(Type<'a>),
    Const(Const<'a>),
    ImplNamed(ImplNamed<'a>),
    ImplAnonymous(ImplAnonymous<'a>),
    NegativeImpl(NegativeImpl<'a>),
}

impl<'a> NodeToElement<'a, ast::GenericParam> for GenericParam<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::GenericParamType => GenericParam::Type(Type::node_to_element(db, node)),
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
