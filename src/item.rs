use crate::function::Function;
use crate::generic_param::option_wrapped_generic_params_to_vec;
use crate::{
    element_list_to_vec, DbSyntaxNode, DbTns, DbTypedSyntaxNode, DynDbSyntaxNode, Expression,
    GenericParam, NewDbTypedSyntaxNode, Visibility,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, Terminal, TypedSyntaxNode};

pub type Constant<'a> = DbTns<'a, ast::ItemConstant>;
pub type Module<'a> = DbTns<'a, ast::ItemModule>;
pub type Use<'a> = DbTns<'a, ast::ItemUse>;
pub type ExternFunction<'a> = DbTns<'a, ast::ItemExternFunction>;
pub type ExternType<'a> = DbTns<'a, ast::ItemExternType>;
pub type Trait<'a> = DbTns<'a, ast::ItemTrait>;
pub type Impl<'a> = DbTns<'a, ast::ItemImpl>;
pub type ImplAlias<'a> = DbTns<'a, ast::ItemImplAlias>;
pub type Struct<'a> = DbTns<'a, ast::ItemStruct>;
pub type Enum<'a> = DbTns<'a, ast::ItemEnum>;
pub type TypeAlias<'a> = DbTns<'a, ast::ItemTypeAlias>;
pub type InlineMacro<'a> = DbTns<'a, ast::ItemInlineMacro>;
pub type ItemHeaderDoc<'a> = DbTns<'a, ast::ItemHeaderDoc>;
pub type ItemMissing<'a> = DbTns<'a, ast::ModuleItemMissing>;

pub type Member<'a> = DbTns<'a, ast::Member>;
pub type Variant<'a> = DbTns<'a, ast::Variant>;
pub type Attribute<'a> = DbTns<'a, ast::Attribute>;
pub type Arg<'a> = DbTns<'a, ast::Arg>;

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

impl<'a> DynDbSyntaxNode<'a> for Item<'a> {
    fn to_dyn_db_ast_trait(&self) -> &dyn DbSyntaxNode {
        match self {
            Item::Constant(item) => item,
            Item::Module(item) => item,
            Item::Use(item) => item,
            Item::FreeFunction(item) => item,
            Item::ExternFunction(item) => item,
            Item::ExternType(item) => item,
            Item::Trait(item) => item,
            Item::Impl(item) => item,
            Item::ImplAlias(item) => item,
            Item::Struct(item) => item,
            Item::Enum(item) => item,
            Item::TypeAlias(item) => item,
            Item::InlineMacro(item) => item,
            Item::HeaderDoc(item) => item,
            Item::Missing(item) => item,
        }
    }
}

impl<'a> NewDbTypedSyntaxNode<'a> for Item<'a> {
    type TSN = ast::ModuleItem;
    fn new(db: &'a dyn SyntaxGroup, node: ast::ModuleItem) -> Self {
        match node {
            ast::ModuleItem::Constant(item) => Item::Constant(Constant::new(db, item)),
            ast::ModuleItem::Module(item) => Item::Module(Module::new(db, item)),
            ast::ModuleItem::Use(item) => Item::Use(Use::new(db, item)),
            ast::ModuleItem::FreeFunction(item) => Item::FreeFunction(Function::new(db, item)),
            ast::ModuleItem::ExternFunction(item) => {
                Item::ExternFunction(ExternFunction::new(db, item))
            }
            ast::ModuleItem::ExternType(item) => Item::ExternType(ExternType::new(db, item)),
            ast::ModuleItem::Trait(item) => Item::Trait(Trait::new(db, item)),
            ast::ModuleItem::Impl(item) => Item::Impl(Impl::new(db, item)),
            ast::ModuleItem::ImplAlias(item) => Item::ImplAlias(ImplAlias::new(db, item)),
            ast::ModuleItem::Struct(item) => Item::Struct(Struct::new(db, item)),
            ast::ModuleItem::Enum(item) => Item::Enum(Enum::new(db, item)),
            ast::ModuleItem::TypeAlias(item) => Item::TypeAlias(TypeAlias::new(db, item)),
            ast::ModuleItem::InlineMacro(item) => Item::InlineMacro(InlineMacro::new(db, item)),
            ast::ModuleItem::HeaderDoc(item) => Item::HeaderDoc(ItemHeaderDoc::new(db, item)),
            ast::ModuleItem::Missing(item) => Item::Missing(ItemMissing::new(db, item)),
        }
    }
}

impl<'a> DbTypedSyntaxNode<'a> for Item<'a> {
    type TSN = ast::ModuleItem;
    fn typed_syntax_node(&self) -> Self::TSN {
        ast::ModuleItem::from_syntax_node(self.db(), self.syntax_node())
    }
}

impl Constant<'_> {
    pub fn attributes(&self) -> Vec<Attribute> {
        element_list_to_vec(self.db(), self.tsn.attributes(self.db()))
    }
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

    pub fn value(&self) -> Expression {
        Expression::new(self.db(), self.tsn.value(self.db()))
    }
}

impl Module<'_> {
    pub fn attributes(&self) -> Vec<Attribute> {
        element_list_to_vec(self.db(), self.tsn.attributes(self.db()))
    }
    pub fn visibility(&self) -> Visibility {
        match self.tsn.visibility(self.db()) {
            ast::Visibility::Pub(_) => Visibility::Pub,
            ast::Visibility::Default(_) => Visibility::Default,
        }
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
    pub fn visibility(&self) -> Visibility {
        match self.tsn.visibility(self.db()) {
            ast::Visibility::Pub(_) => Visibility::Pub,
            ast::Visibility::Default(_) => Visibility::Default,
        }
    }
    pub fn name(&self) -> String {
        self.tsn.name(self.db()).text(self.db()).to_string()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        option_wrapped_generic_params_to_vec(self.db(), self.tsn.generic_params(self.db()))
    }
    pub fn members(&self) -> Vec<Member> {
        let list = self.tsn.members(self.db());
        element_list_to_vec(self.db(), list)
    }
}

impl Enum<'_> {
    pub fn visibility(&self) -> Visibility {
        match self.tsn.visibility(self.db()) {
            ast::Visibility::Pub(_) => Visibility::Pub,
            ast::Visibility::Default(_) => Visibility::Default,
        }
    }
    pub fn name(&self) -> String {
        self.tsn.name(self.db()).text(self.db()).to_string()
    }
    pub fn generic_params(&self) -> Vec<GenericParam> {
        option_wrapped_generic_params_to_vec(self.db(), self.tsn.generic_params(self.db()))
    }
    pub fn variants(&self) -> Vec<Variant> {
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
