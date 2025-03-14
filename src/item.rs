use crate::function::Function;
use crate::generic_param::option_wrapped_generic_params_to_vec;
use crate::syntax_element::ToTypedSyntaxElementLike;
use crate::{CreateElement, Expression, GenericParam, SyntaxElementTrait, TypedSyntaxElement, Visibility};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode, Terminal, Token, TypedSyntaxNode};

pub type Constant<'a> = TypedSyntaxElement<'a, ast::ItemConstant>;
pub type Module<'a> = TypedSyntaxElement<'a, ast::ItemModule>;
pub type Use<'a> = TypedSyntaxElement<'a, ast::ItemUse>;
pub type ExternFunction<'a> = TypedSyntaxElement<'a, ast::ItemExternFunction>;
pub type ExternType<'a> = TypedSyntaxElement<'a, ast::ItemExternType>;
pub type Trait<'a> = TypedSyntaxElement<'a, ast::ItemTrait>;
pub type Impl<'a> = TypedSyntaxElement<'a, ast::ItemImpl>;
pub type ImplAlias<'a> = TypedSyntaxElement<'a, ast::ItemImplAlias>;
pub type Struct<'a> = TypedSyntaxElement<'a, ast::ItemStruct>;
pub type Enum<'a> = TypedSyntaxElement<'a, ast::ItemEnum>;
pub type TypeAlias<'a> = TypedSyntaxElement<'a, ast::ItemTypeAlias>;
pub type InlineMacro<'a> = TypedSyntaxElement<'a, ast::ItemInlineMacro>;
pub type ItemHeaderDoc<'a> = TypedSyntaxElement<'a, ast::ItemHeaderDoc>;
pub type ItemMissing<'a> = TypedSyntaxElement<'a, ast::ModuleItemMissing>;

pub type Member<'a> = TypedSyntaxElement<'a, ast::Member>;
pub type Variant<'a> = TypedSyntaxElement<'a, ast::Variant>;
pub type Attribute<'a> = TypedSyntaxElement<'a, ast::Attribute>;
pub type Arg<'a> = TypedSyntaxElement<'a, ast::Arg>;

pub enum Item<'a> {
    Constant(Constant<'a>),
    Module(Module<'a>),
    Use(Use<'a>),
    FreeFunction(Function<'a>),
    ExternFunction(ExternFunction<'a>),
    ExternType(ExternType<'a>),
    Trait(Trait<'a>),
    Impl(Impl<'a>),
    ImplAlias(ImplAlias<'a>),
    Struct(Struct<'a>),
    Enum(Enum<'a>),
    TypeAlias(TypeAlias<'a>),
    InlineMacro(InlineMacro<'a>),
    HeaderDoc(ItemHeaderDoc<'a>),
    Missing(ItemMissing<'a>),
}

impl<'a> ToTypedSyntaxElementLike<'a> for Item<'a> {
    fn to_typed_syntax_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Item<'a> {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::ItemConstant => Item::Constant(Constant::from_syntax_node(db, node)),
            SyntaxKind::ItemModule => Item::Module(Module::from_syntax_node(db, node)),
            SyntaxKind::ItemUse => Item::Use(Use::from_syntax_node(db, node)),
            SyntaxKind::FunctionWithBody => {
                Item::FreeFunction(Function::from_syntax_node(db, node))
            }
            SyntaxKind::ItemExternFunction => {
                Item::ExternFunction(ExternFunction::from_syntax_node(db, node))
            }
            SyntaxKind::ItemExternType => Item::ExternType(ExternType::from_syntax_node(db, node)),
            SyntaxKind::ItemTrait => Item::Trait(Trait::from_syntax_node(db, node)),
            SyntaxKind::ItemImpl => Item::Impl(Impl::from_syntax_node(db, node)),
            SyntaxKind::ItemImplAlias => Item::ImplAlias(ImplAlias::from_syntax_node(db, node)),
            SyntaxKind::ItemStruct => Item::Struct(Struct::from_syntax_node(db, node)),
            SyntaxKind::ItemEnum => Item::Enum(Enum::from_syntax_node(db, node)),
            SyntaxKind::ItemTypeAlias => Item::TypeAlias(TypeAlias::from_syntax_node(db, node)),
            SyntaxKind::ItemInlineMacro => {
                Item::InlineMacro(InlineMacro::from_syntax_node(db, node))
            }
            SyntaxKind::ItemHeaderDoc => Item::HeaderDoc(ItemHeaderDoc::from_syntax_node(db, node)),
            SyntaxKind::ModuleItemMissing => Item::Missing(ItemMissing::from_syntax_node(db, node)),
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "Item"
            ),
        }
    }
}

impl Constant<'_> {
    pub fn attributes(&self) -> Vec<Attribute> {
        self.get_child_vec::<{ ast::ItemConstant::INDEX_ATTRIBUTES }, 1, Attribute>()
    }
    pub fn visibility(&self) -> Visibility {
        self.get_child_element::<{ ast::ItemConstant::INDEX_VISIBILITY }>()
    }
    pub fn name(&self) -> String {
        let tsn: ast::TerminalIdentifier =
            self.get_child_typed_syntax_node::<{ ast::ItemConstant::INDEX_NAME }>();
        tsn.token(self.db).text(self.db).to_string()
    }
    pub fn ty(&self) -> Expression {
        self.get_child_typed_syntax_element::<{ ast::ItemConstant::INDEX_TYPE_CLAUSE }, _>()
    }

    pub fn value(&self) -> Expression {
        self.get_child_typed_syntax_element::<{ ast::ItemConstant::INDEX_VALUE }, _>()
    }
}

impl Module<'_> {
    pub fn attributes(&self) -> Vec<Attribute> {
        self.get_child_vec::<{ ast::ItemModule::INDEX_ATTRIBUTES }, 1, Attribute>()
    }
    pub fn visibility(&self) -> Visibility {
        self.get_child_element::<{ ast::ItemModule::INDEX_VISIBILITY }>()
    }
    pub fn name(&self) -> String {
        self.tsn.name(self.db()).text(self.db()).to_string()
    }
    pub fn items(&self) -> Vec<Item> {
        match self.tsn.body(self.db()) {
            ast::MaybeModuleBody::None(_) => vec![],
            ast::MaybeModuleBody::Some(tsn) => element_list_to_vec(self.db(), tsn.items(self.db())),
        }
    }
}

impl Struct<'_> {
    pub const INDEX_VISIBILITY: usize = ast::ItemStruct::INDEX_VISIBILITY;
    pub const INDEX_ATTRIBUTES: usize = ast::ItemStruct::INDEX_ATTRIBUTES;
    pub const INDEX_VISIBILITY: usize = ast::ItemStruct::INDEX_VISIBILITY;
    pub const INDEX_STRUCT_KW: usize = ast::ItemStruct::INDEX_STRUCT_KW;
    pub const INDEX_NAME: usize = ast::ItemStruct::INDEX_NAME;
    pub const INDEX_GENERIC_PARAMS: usize = ast::ItemStruct::INDEX_GENERIC_PARAMS;
    pub const INDEX_LBRACE: usize = ast::ItemStruct::INDEX_LBRACE;
    pub const INDEX_MEMBERS: usize = ast::ItemStruct::INDEX_MEMBERS;
    pub const INDEX_RBRACE: usize = ast::ItemStruct::INDEX_RBRACE;
    
    pub fn visibility(&self) -> Visibility {
        self.get_child_element::<{ Self::INDEX_VISIBILITY }>()
    }
    pub fn name(&self) -> String {
        self.tsn.name(self.db()).text(self.db()).to_string()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        option_wrapped_generic_params_to_vec(self.db(), self.tsn.generic_params(self.db()))
    }
    pub fn members(&self) -> Vec<Member> {
        self.get_child_vec::<{ Self::INDEX_MEMBERS }>()
    }
}

impl Enum<'_> {
    pub fn visibility(&self) -> Visibility {
        Visibility::from_parent_typed_syntax_element::<{ast::ItemEnum::INDEX_VISIBILITY}, >(self)
    }
    pub fn name(&self) -> String {
        self.tsn.name(self.db()).text(self.db()).to_string()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        option_wrapped_generic_params_to_vec(self.db(), self.tsn.generic_params(self.db()))
    }
    pub fn variants(&self) -> Vec<Variant> {
        self.get_child_vec::<{ ast::ItemStruct::INDEX_MEMBERS }>()
        element_list_to_vec(self.db(), self.tsn.variants(self.db()))
    }
}

impl Member<'_> {
    pub fn visibility(&self) -> Visibility {
        match self.tsn.visibility(self.db()) {
            ast::Visibility::Pub(_) => Visibility::Pub,
            ast::Visibility::Default(_) => Visibility::Default,
        }
    }
    pub fn name(&self) -> String {
        self.tsn.name(self.db()).text(self.db()).to_string()
    }
    pub fn ty(&self) -> Expression {
        Expression::new(self.db(), self.tsn.type_clause(self.db()).ty(self.db()))
    }
}

impl Variant<'_> {
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
