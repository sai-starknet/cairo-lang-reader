use crate::syntax_element::ToTypedSyntaxElementLike;
use crate::{element_list_to_vec, expression, Expression, TypedSyntaxElement};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::SyntaxNode;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

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

impl Visibility {
    pub fn from_parent_typed_syntax_element<TSN: TypedSyntaxNode>(
        parent: TypedSyntaxElement<TSN>,
        index: usize,
    ) -> Visibility {
        let kind = parent.get_kind_of_child(index);
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

impl Attribute<'_> {
    pub fn attr(&self) -> expression::Path {
        expression::Path::from_syntax_node(self.db, self.node.attr(self.db))
    }
    pub fn arguments(&self) -> Vec<Arg> {
        option_args_parenthesized_to_vec(self.db, self.node.arguments(self.db))
    }
}

impl Param<'_> {
    pub fn modifiers(&self) -> Vec<Modifier> {
        ast_modifiers_to_modifier(self.db, self.node.modifiers(self.db))
    }
    pub fn name(&self) -> String {
        self.node.name(self.db).text(self.db).to_string()
    }
    pub fn ty(&self) -> Option<Expression> {
        self.get_child_typed_syntax_element(2)
    }
}

impl TerminalIdentifier<'_> {
    pub fn text(&self) -> String {
        let tsn: ast::TokenIdentifier = self.get_child_typed_syntax_node(1);
    }
}

pub fn option_args_parenthesized_to_vec<'a>(
    db: &'a dyn SyntaxGroup,
    option_args_parenthesized: ast::OptionArgListParenthesized,
) -> Vec<Arg<'a>> {
    match option_args_parenthesized {
        ast::OptionArgListParenthesized::Empty(_) => vec![],
        ast::OptionArgListParenthesized::ArgListParenthesized(node) => {
            element_list_to_vec(db, node.arguments(db))
        }
    }
}

pub fn ast_modifiers_to_modifier<'a>(
    db: &'a dyn SyntaxGroup,
    modifier_list: ast::ModifierList,
) -> Vec<Modifier> {
    modifier_list
        .elements(db)
        .iter()
        .map(|m| match m {
            ast::Modifier::Ref(_) => Modifier::Ref,
            ast::Modifier::Mut(_) => Modifier::Mut,
        })
        .collect()
}

fn type_clause_to_expression<'a>(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Expression<'a> {
    Expression::to_child_typed_syntax_element(db, node, ast::TypeClause::INDEX_TY)
}
