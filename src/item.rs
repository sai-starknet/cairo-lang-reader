use crate::function::Function;
use crate::{
    Attribute, ElementList, Expression, GenericParam, NodeToElement, SyntaxElementTrait,
    TypedSyntaxElement, Visibility,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode};

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
pub type ModuleBody<'a> = TypedSyntaxElement<'a, ast::ModuleBody>;

impl ElementList for ast::ModuleItemList {
    const STEP: usize = 1;
    type TSN = ast::ModuleItem;
}

impl ElementList for ast::VariantList {
    const STEP: usize = 2;
    type TSN = ast::Variant;
}

impl ElementList for ast::MemberList {
    const STEP: usize = 2;
    type TSN = ast::Member;
}
#[derive(Clone)]
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

impl<'a> NodeToElement<'a, ast::ModuleItem> for Item<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Item<'a> {
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

impl<'a, E> NodeToElement<'a, ast::ModuleBody> for Vec<E>
where
    E: NodeToElement<'a, ast::ModuleItem>,
{
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Vec<E> {
        NodeToElement::<'a, ast::ModuleItemList>::child_node_to_element::<
            { ast::ModuleBody::INDEX_ITEMS },
        >(db, node)
    }
}

impl<'a, E: NodeToElement<'a, ast::ModuleItem>> NodeToElement<'a, ast::MaybeModuleBody> for Vec<E> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Vec<E> {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::TerminalSemicolon => Vec::new(),
            SyntaxKind::ModuleBody => {
                NodeToElement::<'a, ast::ModuleBody>::node_to_element(db, node)
            }
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "MaybeModuleBody"
            ),
        }
    }
}

impl<'a> Constant<'a> {
    pub const INDEX_ATTRIBUTES: usize = ast::ItemConstant::INDEX_ATTRIBUTES;
    pub const INDEX_VISIBILITY: usize = ast::ItemConstant::INDEX_VISIBILITY;
    pub const INDEX_CONST_KW: usize = ast::ItemConstant::INDEX_CONST_KW;
    pub const INDEX_NAME: usize = ast::ItemConstant::INDEX_NAME;
    pub const INDEX_TYPE_CLAUSE: usize = ast::ItemConstant::INDEX_TYPE_CLAUSE;
    pub const INDEX_EQ: usize = ast::ItemConstant::INDEX_EQ;
    pub const INDEX_VALUE: usize = ast::ItemConstant::INDEX_VALUE;
    pub const INDEX_SEMICOLON: usize = ast::ItemConstant::INDEX_SEMICOLON;

    pub fn attributes(&self) -> Vec<Attribute> {
        self.get_child_element::<{ Constant::INDEX_ATTRIBUTES }, ast::AttributeList, Vec<Attribute>>()
    }
    pub fn visibility(&self) -> Visibility {
        self.get_child_element::<{ Constant::INDEX_VISIBILITY }, ast::Visibility, Visibility>()
    }
    // pub fn name(&self) -> String {
    //     let tsn: ast::TerminalIdentifier =
    //         self.get_child_typed_syntax_node::<{ Self::INDEX_NAME }>();
    //     tsn.token(self.db).text(self.db).to_string()
    // }
    pub fn ty(&self) -> Expression {
        self.get_child_element::<{ Constant::INDEX_TYPE_CLAUSE }, ast::TypeClause, Expression<'a>>()
    }

    pub fn value(&self) -> Expression {
        self.get_child_element::<{ Constant::INDEX_VALUE }, ast::Expr, Expression>()
    }
}

impl Module<'_> {
    pub const INDEX_ATTRIBUTES: usize = ast::ItemModule::INDEX_ATTRIBUTES;
    pub const INDEX_VISIBILITY: usize = ast::ItemModule::INDEX_VISIBILITY;
    pub const INDEX_MODULE_KW: usize = ast::ItemModule::INDEX_MODULE_KW;
    pub const INDEX_NAME: usize = ast::ItemModule::INDEX_NAME;
    pub const INDEX_BODY: usize = ast::ItemModule::INDEX_BODY;

    pub fn attributes(&self) -> Vec<Attribute> {
        self.get_child_element::<{ Module::INDEX_ATTRIBUTES }, ast::AttributeList, Vec<Attribute>>()
    }

    pub fn visibility(&self) -> Visibility {
        self.get_child_element::<{ Module::INDEX_VISIBILITY }, ast::Visibility, Visibility>()
    }
    // pub fn name(&self) -> String {
    //     self.tsn.name(self.db()).text(self.db()).to_string()
    // }
    pub fn items(&self) -> Vec<Item> {
        self.get_child_element::<{ Module::INDEX_BODY }, ast::MaybeModuleBody, Vec<Item>>()
    }
}

impl Struct<'_> {
    pub const INDEX_VISIBILITY: usize = ast::ItemStruct::INDEX_VISIBILITY;
    pub const INDEX_ATTRIBUTES: usize = ast::ItemStruct::INDEX_ATTRIBUTES;
    pub const INDEX_STRUCT_KW: usize = ast::ItemStruct::INDEX_STRUCT_KW;
    pub const INDEX_NAME: usize = ast::ItemStruct::INDEX_NAME;
    pub const INDEX_GENERIC_PARAMS: usize = ast::ItemStruct::INDEX_GENERIC_PARAMS;
    pub const INDEX_LBRACE: usize = ast::ItemStruct::INDEX_LBRACE;
    pub const INDEX_MEMBERS: usize = ast::ItemStruct::INDEX_MEMBERS;
    pub const INDEX_RBRACE: usize = ast::ItemStruct::INDEX_RBRACE;

    pub fn visibility(&self) -> Visibility {
        self.get_child_element::<{ Struct::INDEX_VISIBILITY }, ast::Visibility, Visibility>()
    }
    pub fn name(&self) -> String {
        self.get_child_element::<{ Struct::INDEX_NAME }, ast::TerminalIdentifier, String>()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        self.get_child_element::<{Struct::INDEX_GENERIC_PARAMS}, ast::OptionWrappedGenericParamList, _>(
        )
    }
    pub fn members(&self) -> Vec<Member> {
        self.get_child_element::<{ Struct::INDEX_MEMBERS }, ast::MemberList, _>()
    }
}

impl Enum<'_> {
    pub const INDEX_ATTRIBUTES: usize = ast::ItemEnum::INDEX_ATTRIBUTES;
    pub const INDEX_VISIBILITY: usize = ast::ItemEnum::INDEX_VISIBILITY;
    pub const INDEX_ENUM_KW: usize = ast::ItemEnum::INDEX_ENUM_KW;
    pub const INDEX_NAME: usize = ast::ItemEnum::INDEX_NAME;
    pub const INDEX_GENERIC_PARAMS: usize = ast::ItemEnum::INDEX_GENERIC_PARAMS;
    pub const INDEX_LBRACE: usize = ast::ItemEnum::INDEX_LBRACE;
    pub const INDEX_VARIANTS: usize = ast::ItemEnum::INDEX_VARIANTS;
    pub const INDEX_RBRACE: usize = ast::ItemEnum::INDEX_RBRACE;

    pub fn visibility(&self) -> Visibility {
        self.get_child_element::<{ Enum::INDEX_VISIBILITY }, ast::Visibility, Visibility>()
    }
    pub fn name(&self) -> String {
        self.get_child_element::<{ Enum::INDEX_NAME }, ast::TerminalIdentifier, String>()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        self.get_child_element::<{Enum::INDEX_GENERIC_PARAMS}, ast::OptionWrappedGenericParamList, _>(
        )
    }
    pub fn variants(&self) -> Vec<Variant> {
        self.get_child_element::<{ Enum::INDEX_VARIANTS }, ast::VariantList, Vec<Variant>>()
    }
}

impl Member<'_> {
    pub const INDEX_ATTRIBUTES: usize = ast::Member::INDEX_ATTRIBUTES;
    pub const INDEX_VISIBILITY: usize = ast::Member::INDEX_VISIBILITY;
    pub const INDEX_NAME: usize = ast::Member::INDEX_NAME;
    pub const INDEX_TYPE_CLAUSE: usize = ast::Member::INDEX_TYPE_CLAUSE;
    pub fn visibility(&self) -> Visibility {
        self.get_child_element::<{ Member::INDEX_VISIBILITY }, ast::Visibility, Visibility>()
    }
    pub fn name(&self) -> String {
        self.get_child_element::<{ Member::INDEX_NAME }, ast::TerminalIdentifier, String>()
    }
    pub fn ty(&self) -> Expression {
        self.get_child_element::<{ Member::INDEX_TYPE_CLAUSE }, ast::TypeClause, Expression>()
    }
}

impl Variant<'_> {
    pub const INDEX_ATTRIBUTES: usize = ast::Variant::INDEX_ATTRIBUTES;
    pub const INDEX_NAME: usize = ast::Variant::INDEX_NAME;
    pub const INDEX_TYPE_CLAUSE: usize = ast::Variant::INDEX_TYPE_CLAUSE;
    pub fn name(&self) -> String {
        self.get_child_element::<{ Variant::INDEX_NAME }, ast::TerminalIdentifier, String>()
    }

    pub fn ty(&self) -> Option<Expression> {
        self.get_child_element::<{ Variant::INDEX_TYPE_CLAUSE }, ast::OptionTypeClause, Option<Expression>>()
    }
}
