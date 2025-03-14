use crate::TypedSyntaxElement;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

pub type Type<'a> = TypedSyntaxElement<'a, ast::GenericParamType>;
pub type Const<'a> = TypedSyntaxElement<'a, ast::GenericParamConst>;
pub type ImplNamed<'a> = TypedSyntaxElement<'a, ast::GenericParamImplNamed>;
pub type ImplAnonymous<'a> = TypedSyntaxElement<'a, ast::GenericParamImplAnonymous>;
pub type NegativeImpl<'a> = TypedSyntaxElement<'a, ast::GenericParamNegativeImpl>;

pub enum GenericParam<'a> {
    Type(Type<'a>),
    Const(Const<'a>),
    ImplNamed(ImplNamed<'a>),
    ImplAnonymous(ImplAnonymous<'a>),
    NegativeImpl(NegativeImpl<'a>),
}

impl<'a> DynDbSyntaxNode<'a> for GenericParam<'a> {
    fn to_dyn_db_ast_trait(&self) -> &dyn DbSyntaxNode {
        match self {
            GenericParam::Type(item) => item,
            GenericParam::Const(item) => item,
            GenericParam::ImplNamed(item) => item,
            GenericParam::ImplAnonymous(item) => item,
            GenericParam::NegativeImpl(item) => item,
        }
    }
}

impl<'a> NewDbTypedSyntaxNode<'a> for GenericParam<'a> {
    type TSN = ast::GenericParam;
    fn new(db: &'a dyn SyntaxGroup, node: ast::GenericParam) -> Self {
        match node {
            ast::GenericParam::Type(item) => GenericParam::Type(Type::new(db, item)),
            ast::GenericParam::Const(item) => GenericParam::Const(Const::new(db, item)),
            ast::GenericParam::ImplNamed(item) => GenericParam::ImplNamed(ImplNamed::new(db, item)),
            ast::GenericParam::ImplAnonymous(item) => {
                GenericParam::ImplAnonymous(ImplAnonymous::new(db, item))
            }
            ast::GenericParam::NegativeImpl(item) => {
                GenericParam::NegativeImpl(NegativeImpl::new(db, item))
            }
        }
    }
}

impl<'a> DbTypedSyntaxNode<'a> for GenericParam<'a> {
    type TSN = ast::GenericParam;
    fn typed_syntax_node(&self) -> Self::TSN {
        ast::GenericParam::from_syntax_node(self.db(), self.syntax_node())
    }
}

pub fn option_wrapped_generic_params_to_vec<'a>(
    db: &'a dyn SyntaxGroup,
    option_wrapped_generic_params: ast::OptionWrappedGenericParamList,
) -> Vec<GenericParam<'a>> {
    match option_wrapped_generic_params {
        ast::OptionWrappedGenericParamList::Empty(_) => vec![],
        ast::OptionWrappedGenericParamList::WrappedGenericParamList(tsn) => {
            let list = tsn.generic_params(db);
            element_list_to_vec(db, list)
        }
    }
}
