use crate::{
    expression, ElementList, Expression, NodeToElement, SyntaxElementTrait, TypedSyntaxElement,
};
use cairo_lang_syntax::node::ast;
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::SyntaxNode;

pub type Attribute<'a> = TypedSyntaxElement<'a, ast::Attribute>;
pub type Arg<'a> = TypedSyntaxElement<'a, ast::Arg>;
pub type Param<'a> = TypedSyntaxElement<'a, ast::Param>;
pub type TerminalIdentifier<'a> = TypedSyntaxElement<'a, ast::TerminalIdentifier>;
pub type TokenIdentifier<'a> = TypedSyntaxElement<'a, ast::TokenIdentifier>;

pub type ArgClauseUnnamed<'a> = TypedSyntaxElement<'a, ast::ArgClauseUnnamed>;
pub type ArgClauseNamed<'a> = TypedSyntaxElement<'a, ast::ArgClauseNamed>;
pub type ArgClauseFieldInitShorthand<'a> = TypedSyntaxElement<'a, ast::ArgClauseFieldInitShorthand>;

impl ElementList for ast::ArgList {
    const STEP: usize = 2;
    type TSN = ast::Arg;
}

impl ElementList for ast::ModifierList {
    const STEP: usize = 1;
    type TSN = ast::Modifier;
}

impl ElementList for ast::AttributeList {
    const STEP: usize = 1;
    type TSN = ast::Attribute;
}

impl ElementList for ast::ParamList {
    const STEP: usize = 2;
    type TSN = ast::Param;
}

#[derive(Clone, Debug)]
pub enum Visibility {
    Default,
    Pub,
}

pub enum Modifier {
    Ref,
    Mut,
}

pub enum ArgClause<'a> {
    Unnamed(ArgClauseUnnamed<'a>),
    Named(ArgClauseNamed<'a>),
    FieldInitShorthand(ArgClauseFieldInitShorthand<'a>),
}

impl<'a> NodeToElement<'a, ast::ArgClauseUnnamed> for Expression<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        NodeToElement::<'_, ast::ArgClauseUnnamed>::child_node_to_element::<
            { ast::ArgClauseUnnamed::INDEX_VALUE },
        >(db, node)
    }
}

impl NodeToElement<'_, ast::ExprFieldInitShorthand> for String {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> String {
        NodeToElement::<'_, ast::TerminalIdentifier>::child_node_to_element::<
            { ast::ExprFieldInitShorthand::INDEX_NAME },
        >(db, node)
    }
}

impl NodeToElement<'_, ast::ArgClauseFieldInitShorthand> for String {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> String {
        NodeToElement::<'_, ast::ArgClauseFieldInitShorthand>::child_node_to_element::<
            { ast::ArgClauseFieldInitShorthand::INDEX_NAME },
        >(db, node)
    }
}

impl NodeToElement<'_, ast::Visibility> for Visibility {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> Visibility {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::VisibilityDefault => Visibility::Default,
            SyntaxKind::VisibilityPub => Visibility::Pub,
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "Visibility"
            ),
        }
    }
}

impl NodeToElement<'_, ast::Visibility> for bool {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> bool {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::VisibilityDefault => false,
            SyntaxKind::VisibilityPub => true,
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

impl<'a> NodeToElement<'a, ast::ArgClause> for ArgClause<'a> {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> ArgClause {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::ArgClauseUnnamed => {
                ArgClause::Unnamed(NodeToElement::<'_, ast::ArgClauseUnnamed>::node_to_element(
                    db, node,
                ))
            }
            SyntaxKind::ArgClauseNamed => ArgClause::Named(
                NodeToElement::<'_, ast::ArgClauseNamed>::node_to_element(db, node),
            ),
            SyntaxKind::ArgClauseFieldInitShorthand => ArgClause::FieldInitShorthand(
                NodeToElement::<'_, ast::ArgClauseFieldInitShorthand>::node_to_element(db, node),
            ),
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "ArgClause"
            ),
        }
    }
}


impl Visibility {
    pub fn to_string(&self) -> String {
        match self {
            Visibility::Default => "".into(),
            Visibility::Pub => "pub".into(),
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
    pub fn name(&self) -> String {
        self.get_child_element::<{ Param::INDEX_NAME }, ast::TerminalIdentifier, String>()
    }
    pub fn ty(&self) -> Option<Expression> {
        self.get_child_element::<{ Param::INDEX_TYPE_CLAUSE }, ast::OptionTypeClause, _>()
    }
}

impl Arg<'_> {
    pub const INDEX_MODIFIERS: usize = ast::Arg::INDEX_MODIFIERS;
    pub const INDEX_ARG_CLAUSE: usize = ast::Arg::INDEX_ARG_CLAUSE;

    pub fn modifiers(&self) -> Vec<Modifier> {
        self.get_child_element::<{ Arg::INDEX_MODIFIERS }, ast::ModifierList, _>()
    }
    pub fn arg_clause(&self) -> ArgClause {
        self.get_child_element::<{ Arg::INDEX_ARG_CLAUSE }, ast::ArgClause, _>()
    }
}

impl<'a> ArgClauseNamed<'a> {
    pub const INDEX_NAME: usize = ast::ArgClauseNamed::INDEX_NAME;
    pub const INDEX_COLON: usize = ast::ArgClauseNamed::INDEX_COLON;
    pub const INDEX_VALUE: usize = ast::ArgClauseNamed::INDEX_VALUE;

    pub fn name(&self) -> String {
        self.get_child_element::<{ ArgClauseNamed::INDEX_NAME }, ast::TerminalIdentifier, _>()
    }
    pub fn value(&self) -> Expression<'a> {
        self.get_child_element::<{ ArgClauseNamed::INDEX_VALUE }, ast::Expr, _>()
    }
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
