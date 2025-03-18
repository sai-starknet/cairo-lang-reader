use crate::syntax_element::get_child;
use crate::{
    expression, ElementList, Expression, NodeToElement, SyntaxElementTrait, TypedSyntaxElement,
};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, Terminal, Token};
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};

pub type Attribute<'a> = TypedSyntaxElement<'a, ast::Attribute>;
pub type Arg<'a> = TypedSyntaxElement<'a, ast::Arg>;
pub type Param<'a> = TypedSyntaxElement<'a, ast::Param>;
pub type TerminalIdentifier<'a> = TypedSyntaxElement<'a, ast::TerminalIdentifier>;
pub type TokenIdentifier<'a> = TypedSyntaxElement<'a, ast::TokenIdentifier>;

#[derive(Clone, Debug)]
pub enum Visibility {
    Default,
    Pub,
}

pub enum Modifier {
    Ref,
    Mut,
}

impl NodeToElement<'_, ast::Visibility> for Visibility {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> Visibility {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::VisibilityDefault => Visibility::Pub,
            SyntaxKind::VisibilityPub => Visibility::Default,
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "Visibility"
            ),
        }
    }
}

impl NodeToElement<'_, ast::Modifier> for Modifier {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> Modifier {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::TerminalRef => Modifier::Ref,
            SyntaxKind::TerminalMut => Modifier::Mut,
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "Modifier"
            ),
        }
    }
}

impl Attribute<'_> {
    pub const INDEX_HASH: usize = ast::Attribute::INDEX_HASH;
    pub const INDEX_LBRACK: usize = ast::Attribute::INDEX_LBRACK;
    pub const INDEX_ATTR: usize = ast::Attribute::INDEX_ATTR;
    pub const INDEX_ARGUMENTS: usize = ast::Attribute::INDEX_ARGUMENTS;
    pub const INDEX_RBRACK: usize = ast::Attribute::INDEX_RBRACK;
    pub fn attr(&self) -> expression::Path {
        self.get_child_element::<{ Attribute::INDEX_ATTR }, ast::ExprPath, expression::Path>()
    }
    pub fn arguments(&self) -> Vec<Arg> {
        self.get_child_element::<{Attribute::INDEX_ARGUMENTS}, ast::OptionArgListParenthesized, _>()
    }
}

impl Param<'_> {
    pub const INDEX_MODIFIERS: usize = ast::Param::INDEX_MODIFIERS;
    pub const INDEX_NAME: usize = ast::Param::INDEX_NAME;
    pub const INDEX_TYPE_CLAUSE: usize = ast::Param::INDEX_TYPE_CLAUSE;

    pub fn modifiers(&self) -> Vec<Modifier> {
        self.get_child_element::<{ Param::INDEX_MODIFIERS }, ast::ModifierList, _>()
    }
    // pub fn name(&self) -> String {
    //     // self.node.name(self.db).text(self.db).to_string()
    // }
    pub fn ty(&self) -> Option<Expression> {
        self.get_child_element::<{ Param::INDEX_TYPE_CLAUSE }, ast::OptionTypeClause, _>()
    }
}

impl Arg<'_> {
    pub const INDEX_MODIFIERS: usize = ast::Arg::INDEX_MODIFIERS;
    pub const INDEX_ARG_CLAUSE: usize = ast::Arg::INDEX_ARG_CLAUSE;
    pub const LIST_STEP: usize = 2;

    pub fn modifiers(&self) -> Vec<Modifier> {
        self.get_child_element::<{ Arg::INDEX_MODIFIERS }, ast::ModifierList, _>()
    }
    // pub fn arg_clause(&self, db: &dyn SyntaxGroup) -> ArgClause {
    //     ArgClause::from_syntax_node(db, self.children[1].clone())
    // }
}

impl<'a> NodeToElement<'a, ast::OptionArgListParenthesized> for Vec<Arg<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::OptionArgListParenthesizedEmpty => vec![],
            SyntaxKind::ArgListParenthesized => {
                NodeToElement::<'a, ast::ArgListParenthesized>::node_to_element(db, node)
            }
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "OptionArgListParenthesized"
            ),
        }
    }
}

impl<'a> NodeToElement<'a, ast::ArgListParenthesized> for Vec<Arg<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'a, ast::ArgList>::child_node_to_element::<
            { ast::ArgListParenthesized::INDEX_ARGUMENTS },
        >(db, node)
    }
}

impl<'a> NodeToElement<'a, ast::ArgList> for Vec<Arg<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        ElementList::<ast::Arg, 2, Arg<'a>>::elements(db, node)
    }
}

impl<'a> NodeToElement<'a, ast::ModifierList> for Vec<Modifier> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        ElementList::<ast::Modifier, 1, Modifier>::elements(db, node)
    }
}

impl<'a, T> NodeToElement<'a, T> for String
where
    T: Terminal,
{
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> String {
        T::TokenType::from_syntax_node(db, get_child::<1>(db, node))
            .text(db)
            .to_string()
    }
}
