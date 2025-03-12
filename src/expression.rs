use super::{DbSyntaxNode, DbTns, DbTypedSyntaxNode, DynDbSyntaxNode, NewDbTypedSyntaxNode};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

pub type Path<'a> = DbTns<'a, ast::ExprPath>;
pub type Literal<'a> = DbTns<'a, ast::TerminalLiteralNumber>;
pub type ShortString<'a> = DbTns<'a, ast::TerminalShortString>;
pub type ExprString<'a> = DbTns<'a, ast::TerminalString>;
pub type False<'a> = DbTns<'a, ast::TerminalFalse>;
pub type True<'a> = DbTns<'a, ast::TerminalTrue>;
pub type Parenthesized<'a> = DbTns<'a, ast::ExprParenthesized>;
pub type Unary<'a> = DbTns<'a, ast::ExprUnary>;
pub type Binary<'a> = DbTns<'a, ast::ExprBinary>;
pub type Tuple<'a> = DbTns<'a, ast::ExprList>;
pub type FunctionCall<'a> = DbTns<'a, ast::ExprFunctionCall>;
pub type StructCtorCall<'a> = DbTns<'a, ast::ExprStructCtorCall>;
pub type Block<'a> = DbTns<'a, ast::ExprBlock>;
pub type Match<'a> = DbTns<'a, ast::ExprMatch>;
pub type If<'a> = DbTns<'a, ast::ExprIf>;
pub type Loop<'a> = DbTns<'a, ast::ExprLoop>;
pub type While<'a> = DbTns<'a, ast::ExprWhile>;
pub type For<'a> = DbTns<'a, ast::ExprFor>;
pub type Closure<'a> = DbTns<'a, ast::ExprClosure>;
pub type ErrorPropagate<'a> = DbTns<'a, ast::ExprErrorPropagate>;
pub type FieldInitShorthand<'a> = DbTns<'a, ast::ExprFieldInitShorthand>;
pub type Indexed<'a> = DbTns<'a, ast::ExprIndexed>;
pub type ExprInlineMacro<'a> = DbTns<'a, ast::ExprInlineMacro>;
pub type FixedSizeArray<'a> = DbTns<'a, ast::ExprFixedSizeArray>;
pub type ExprMissing<'a> = DbTns<'a, ast::ExprMissing>;

pub enum Expression<'a> {
    Path(Path<'a>),
    Literal(Literal<'a>),
    ShortString(ShortString<'a>),
    String(ExprString<'a>),
    False(False<'a>),
    True(True<'a>),
    Parenthesized(Parenthesized<'a>),
    Unary(Unary<'a>),
    Binary(Binary<'a>),
    Tuple(Tuple<'a>),
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
}

impl<'a> DynDbSyntaxNode<'a> for Expression<'a> {
    fn to_dyn_db_ast_trait(&self) -> &dyn DbSyntaxNode {
        match self {
            Expression::Path(expr) => expr,
            Expression::Literal(expr) => expr,
            Expression::ShortString(expr) => expr,
            Expression::String(expr) => expr,
            Expression::False(expr) => expr,
            Expression::True(expr) => expr,
            Expression::Parenthesized(expr) => expr,
            Expression::Unary(expr) => expr,
            Expression::Binary(expr) => expr,
            Expression::Tuple(expr) => expr,
            Expression::FunctionCall(expr) => expr,
            Expression::StructCtorCall(expr) => expr,
            Expression::Block(expr) => expr,
            Expression::Match(expr) => expr,
            Expression::If(expr) => expr,
            Expression::Loop(expr) => expr,
            Expression::While(expr) => expr,
            Expression::For(expr) => expr,
            Expression::Closure(expr) => expr,
            Expression::ErrorPropagate(expr) => expr,
            Expression::FieldInitShorthand(expr) => expr,
            Expression::Indexed(expr) => expr,
            Expression::InlineMacro(expr) => expr,
            Expression::FixedSizeArray(expr) => expr,
            Expression::Missing(expr) => expr,
        }
    }
}

impl<'a> NewDbTypedSyntaxNode<'a> for Expression<'a> {
    type TSN = ast::Expr;
    fn new(db: &'a dyn SyntaxGroup, node: ast::Expr) -> Expression<'a> {
        match node {
            ast::Expr::Path(expr) => Expression::Path(Path::new(db, expr)),
            ast::Expr::Literal(expr) => Expression::Literal(Literal::new(db, expr)),
            ast::Expr::ShortString(expr) => Expression::ShortString(ShortString::new(db, expr)),
            ast::Expr::String(expr) => Expression::String(ExprString::new(db, expr)),
            ast::Expr::False(expr) => Expression::False(False::new(db, expr)),
            ast::Expr::True(expr) => Expression::True(True::new(db, expr)),
            ast::Expr::Parenthesized(expr) => {
                Expression::Parenthesized(Parenthesized::new(db, expr))
            }
            ast::Expr::Unary(expr) => Expression::Unary(Unary::new(db, expr)),
            ast::Expr::Binary(expr) => Expression::Binary(Binary::new(db, expr)),
            ast::Expr::Tuple(expr) => Expression::Tuple(Tuple::new(db, expr.expressions(db))),
            ast::Expr::FunctionCall(expr) => Expression::FunctionCall(FunctionCall::new(db, expr)),
            ast::Expr::StructCtorCall(expr) => {
                Expression::StructCtorCall(StructCtorCall::new(db, expr))
            }
            ast::Expr::Block(expr) => Expression::Block(Block::new(db, expr)),
            ast::Expr::Match(expr) => Expression::Match(Match::new(db, expr)),
            ast::Expr::If(expr) => Expression::If(If::new(db, expr)),
            ast::Expr::Loop(expr) => Expression::Loop(Loop::new(db, expr)),
            ast::Expr::While(expr) => Expression::While(While::new(db, expr)),
            ast::Expr::For(expr) => Expression::For(For::new(db, expr)),
            ast::Expr::Closure(expr) => Expression::Closure(Closure::new(db, expr)),
            ast::Expr::ErrorPropagate(expr) => {
                Expression::ErrorPropagate(ErrorPropagate::new(db, expr))
            }
            ast::Expr::FieldInitShorthand(expr) => {
                Expression::FieldInitShorthand(FieldInitShorthand::new(db, expr))
            }
            ast::Expr::Indexed(expr) => Expression::Indexed(Indexed::new(db, expr)),
            ast::Expr::InlineMacro(expr) => Expression::InlineMacro(ExprInlineMacro::new(db, expr)),
            ast::Expr::FixedSizeArray(expr) => {
                Expression::FixedSizeArray(FixedSizeArray::new(db, expr))
            }
            ast::Expr::Missing(expr) => Expression::Missing(ExprMissing::new(db, expr)),
        }
    }
}

impl<'a> DbTypedSyntaxNode<'a> for Expression<'a> {
    type TSN = ast::Expr;
    fn typed_syntax_node(&self) -> ast::Expr {
        ast::Expr::from_syntax_node(self.db(), self.syntax_node())
    }
}
