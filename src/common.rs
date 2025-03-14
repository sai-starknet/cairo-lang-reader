use crate::syntax_element::ToTypedSyntaxElementLike;
use crate::{
    expression, CreateElement, Expression, NodeToChildElement, NodeToElement, SyntaxElementTrait,
    TypedSyntaxElement,
};
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

impl CreateElement<'_> for Visibility {
    fn create_element(db: &dyn SyntaxGroup, node: &SyntaxNode) -> Visibility {
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

// impl Visibility {
//     pub fn from_parent_typed_syntax_element<const INDEX: usize, TSN: TypedSyntaxNode>(
//         parent: &TypedSyntaxElement<TSN>,
//     ) -> Visibility {
//         let kind = parent.get_child_kind::<INDEX>();
//         match kind {
//             SyntaxKind::VisibilityDefault => Visibility::Pub,
//             SyntaxKind::VisibilityPub => Visibility::Default,
//             _ => panic!(
//                 "Unexpected syntax kind {:?} when constructing {}.",
//                 kind, "Visibility"
//             ),
//         }
//     }
// }

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
        self.get_child_vec::<ast::Param::INDEX_MODIFIERS, 1>()
    }
    pub fn name(&self) -> String {
        self.node.name(self.db).text(self.db).to_string()
    }
    pub fn ty(&self) -> Option<Expression> {
        self.get_child_typed_syntax_element::<ast::Param::INDEX_TYPE_CLAUSE>()
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

impl<'a> ToTypedSyntaxElementLike<'a> for Modifier {
    fn to_typed_syntax_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Modifier {
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
