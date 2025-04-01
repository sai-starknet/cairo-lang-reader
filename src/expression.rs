use crate::{ElementList, NodeToElement, SyntaxElementTrait, TypedSyntaxElement};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode};

pub type Path<'a> = TypedSyntaxElement<'a, ast::ExprPath>;
pub type Literal<'a> = TypedSyntaxElement<'a, ast::TerminalLiteralNumber>;
pub type ShortString<'a> = TypedSyntaxElement<'a, ast::TerminalShortString>;
pub type ExprString<'a> = TypedSyntaxElement<'a, ast::TerminalString>;
pub type False<'a> = TypedSyntaxElement<'a, ast::TerminalFalse>;
pub type True<'a> = TypedSyntaxElement<'a, ast::TerminalTrue>;
pub type Parenthesized<'a> = TypedSyntaxElement<'a, ast::ExprParenthesized>;
pub type Unary<'a> = TypedSyntaxElement<'a, ast::ExprUnary>;
pub type Binary<'a> = TypedSyntaxElement<'a, ast::ExprBinary>;
pub type Tuple<'a> = TypedSyntaxElement<'a, ast::ExprList>;
pub type FunctionCall<'a> = TypedSyntaxElement<'a, ast::ExprFunctionCall>;
pub type StructCtorCall<'a> = TypedSyntaxElement<'a, ast::ExprStructCtorCall>;
pub type Block<'a> = TypedSyntaxElement<'a, ast::ExprBlock>;
pub type Match<'a> = TypedSyntaxElement<'a, ast::ExprMatch>;
pub type If<'a> = TypedSyntaxElement<'a, ast::ExprIf>;
pub type Loop<'a> = TypedSyntaxElement<'a, ast::ExprLoop>;
pub type While<'a> = TypedSyntaxElement<'a, ast::ExprWhile>;
pub type For<'a> = TypedSyntaxElement<'a, ast::ExprFor>;
pub type Closure<'a> = TypedSyntaxElement<'a, ast::ExprClosure>;
pub type ErrorPropagate<'a> = TypedSyntaxElement<'a, ast::ExprErrorPropagate>;
pub type FieldInitShorthand<'a> = TypedSyntaxElement<'a, ast::ExprFieldInitShorthand>;
pub type Indexed<'a> = TypedSyntaxElement<'a, ast::ExprIndexed>;
pub type ExprInlineMacro<'a> = TypedSyntaxElement<'a, ast::ExprInlineMacro>;
pub type FixedSizeArray<'a> = TypedSyntaxElement<'a, ast::ExprFixedSizeArray>;
pub type ExprMissing<'a> = TypedSyntaxElement<'a, ast::ExprMissing>;

pub enum Expression<'a> {
    Path(Path<'a>),
    Literal(Literal<'a>),
    ShortString(String),
    String(String),
    False,
    True,
    Parenthesized(Parenthesized<'a>),
    Unary(Unary<'a>),
    Binary(Binary<'a>),
    Tuple(Vec<Expression<'a>>),
    FunctionCall(FunctionCall<'a>),
    StructCtorCall(StructCtorCall<'a>),
    Block(Block<'a>),
    Match(Match<'a>),
    If(If<'a>),
    Loop(Loop<'a>),
    While(While<'a>),
    For(For<'a>),
    Closure(Closure<'a>),
    ErrorPropagate(ErrorPropagate<'a>),
    FieldInitShorthand(FieldInitShorthand<'a>),
    Indexed(Indexed<'a>),
    InlineMacro(ExprInlineMacro<'a>),
    FixedSizeArray(FixedSizeArray<'a>),
    Missing(ExprMissing<'a>),
    Underscore,
}

impl ElementList for ast::ExprList {
    const STEP: usize = 2;
    type TSN = ast::Expr;
}

impl<'a> NodeToElement<'a, ast::Expr> for Expression<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Expression<'a> {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::ExprPath => Expression::Path(Path::from_syntax_node(db, node)),
            SyntaxKind::TerminalLiteralNumber => {
                Expression::Literal(Literal::from_syntax_node(db, node))
            }
            SyntaxKind::TerminalShortString => {
                Expression::ShortString(NodeToElement::<ast::TerminalShortString>::node_to_element(
                    db, node,
                ))
            }
            SyntaxKind::TerminalString => Expression::String(
                NodeToElement::<ast::TerminalString>::node_to_element(db, node),
            ),
            SyntaxKind::TerminalFalse => Expression::False,
            SyntaxKind::TerminalTrue => Expression::True,
            SyntaxKind::ExprParenthesized => {
                Expression::Parenthesized(Parenthesized::from_syntax_node(db, node))
            }
            SyntaxKind::ExprUnary => Expression::Unary(Unary::from_syntax_node(db, node)),
            SyntaxKind::ExprBinary => Expression::Binary(Binary::from_syntax_node(db, node)),
            SyntaxKind::ExprListParenthesized => {
                Expression::Tuple(
                    NodeToElement::<'a, ast::ExprListParenthesized>::node_to_element(db, node),
                )
            }
            SyntaxKind::ExprFunctionCall => {
                Expression::FunctionCall(FunctionCall::from_syntax_node(db, node))
            }
            SyntaxKind::ExprStructCtorCall => {
                Expression::StructCtorCall(StructCtorCall::from_syntax_node(db, node))
            }
            SyntaxKind::ExprBlock => Expression::Block(Block::from_syntax_node(db, node)),
            SyntaxKind::ExprMatch => Expression::Match(Match::from_syntax_node(db, node)),
            SyntaxKind::ExprIf => Expression::If(If::from_syntax_node(db, node)),
            SyntaxKind::ExprLoop => Expression::Loop(Loop::from_syntax_node(db, node)),
            SyntaxKind::ExprWhile => Expression::While(While::from_syntax_node(db, node)),
            SyntaxKind::ExprFor => Expression::For(For::from_syntax_node(db, node)),
            SyntaxKind::ExprClosure => Expression::Closure(Closure::from_syntax_node(db, node)),
            SyntaxKind::ExprErrorPropagate => {
                Expression::ErrorPropagate(ErrorPropagate::from_syntax_node(db, node))
            }
            SyntaxKind::ExprFieldInitShorthand => {
                Expression::FieldInitShorthand(FieldInitShorthand::from_syntax_node(db, node))
            }
            SyntaxKind::ExprIndexed => Expression::Indexed(Indexed::from_syntax_node(db, node)),
            SyntaxKind::ExprInlineMacro => {
                Expression::InlineMacro(ExprInlineMacro::from_syntax_node(db, node))
            }
            SyntaxKind::ExprFixedSizeArray => {
                Expression::FixedSizeArray(FixedSizeArray::from_syntax_node(db, node))
            }
            SyntaxKind::ExprMissing => Expression::Missing(ExprMissing::from_syntax_node(db, node)),
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "Expression"
            ),
        }
    }
}

impl<'a> NodeToElement<'a, ast::TypeClause> for Expression<'a> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Expression<'a> {
        NodeToElement::<'a, ast::Expr>::child_node_to_element::<{ ast::TypeClause::INDEX_TY }>(
            db, node,
        )
    }
}

impl<'a> NodeToElement<'a, ast::OptionTypeClause> for Option<Expression<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Option<Expression<'a>> {
        let kind = node.kind(db);
        match kind {
            SyntaxKind::OptionTypeClauseEmpty => None,
            SyntaxKind::TypeClause => {
                Some(NodeToElement::<'a, ast::Expr>::node_to_element(db, node))
            }
            _ => panic!(
                "Unexpected syntax kind {:?} when constructing {}.",
                kind, "OptionTypeClause"
            ),
        }
    }
}

impl<'a> NodeToElement<'a, ast::ExprListParenthesized> for Vec<Expression<'a>> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Vec<Expression<'a>> {
        NodeToElement::<'a, ast::ExprList>::child_node_to_element::<
            { ast::ExprListParenthesized::INDEX_EXPRESSIONS },
        >(db, node)
    }
}
