use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_filesystem::span::{TextOffset, TextPosition, TextSpan, TextWidth};
use cairo_lang_syntax::attribute::structured::{AttributeArgVariant, AttributeStructurize};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::element_list::ElementList as AstElementList;
use cairo_lang_syntax::node::green::GreenNode;
use cairo_lang_syntax::node::helpers::QueryAttrs;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode, Terminal, TypedSyntaxNode};
use smol_str::SmolStr;
use std::ops::Deref;
use std::sync::Arc;
pub trait NewDbTypedSyntaxNode<'a> {
    type TSN: TypedSyntaxNode;
    fn new(db: &'a dyn SyntaxGroup, node: Self::TSN) -> Self;
}

pub trait DbSyntaxNode<'a> {
    fn db(&self) -> &dyn SyntaxGroup;
    fn syntax_node(&self) -> SyntaxNode;
    fn offset(&self) -> TextOffset {
        self.syntax_node().offset()
    }
    fn width(&self) -> TextWidth {
        self.syntax_node().width(self.db())
    }
    fn kind(&self) -> SyntaxKind {
        self.syntax_node().kind(self.db())
    }
    fn span(&self) -> TextSpan {
        self.syntax_node().span(self.db())
    }
    fn text(&self) -> Option<SmolStr> {
        self.syntax_node().text(self.db())
    }
    fn green_node(&self) -> Arc<GreenNode> {
        self.syntax_node().green_node(self.db())
    }
    fn span_without_trivia(&self) -> TextSpan {
        self.syntax_node().span_without_trivia(self.db())
    }
    fn position_in_parent(&self) -> Option<usize> {
        self.syntax_node().position_in_parent(self.db())
    }
    fn get_terminal_token(&self) -> Option<SyntaxNode> {
        self.syntax_node().get_terminal_token(self.db())
    }
    fn span_start_without_trivia(&self) -> TextOffset {
        self.syntax_node().span_start_without_trivia(self.db())
    }
    fn span_end_without_trivia(&self) -> TextOffset {
        self.syntax_node().span_end_without_trivia(self.db())
    }
    fn lookup_offset(&self, offset: TextOffset) -> SyntaxNode {
        self.syntax_node().lookup_offset(self.db(), offset)
    }
    fn lookup_position(&self, position: TextPosition) -> SyntaxNode {
        self.syntax_node().lookup_position(self.db(), position)
    }
    fn get_text(&self) -> String {
        self.syntax_node().get_text(self.db())
    }
    fn get_text_without_inner_commentable_children(&self) -> String {
        self.syntax_node()
            .get_text_without_inner_commentable_children(self.db())
    }
    fn get_text_without_all_comment_trivia(&self) -> String {
        self.syntax_node()
            .get_text_without_all_comment_trivia(self.db())
    }
    fn get_text_without_trivia(&self) -> String {
        self.syntax_node().get_text_without_trivia(self.db())
    }
    fn get_text_of_span(&self, span: TextSpan) -> String {
        self.syntax_node().get_text_of_span(self.db(), span)
    }
    fn patch_builder(&self) -> PatchBuilder {
        PatchBuilder::new_ex(self.db(), &self.syntax_node())
    }
}

pub trait DbTypedSyntaxNode<'a>: DbSyntaxNode<'a> {
    type TSN: TypedSyntaxNode;
    fn typed_syntax_node(&self) -> Self::TSN;
    fn missing(&self) -> <Self::TSN as TypedSyntaxNode>::Green {
        Self::TSN::missing(self.db())
    }
    fn stable_ptr(&self) -> <Self::TSN as TypedSyntaxNode>::StablePtr {
        self.typed_syntax_node().stable_ptr()
    }
}

pub trait DynDbSyntaxNode<'a> {
    fn to_dyn_db_ast_trait(&self) -> &dyn DbSyntaxNode;
}

impl<'a, T> DbSyntaxNode<'a> for T
where
    T: DynDbSyntaxNode<'a>,
{
    fn db(&self) -> &dyn SyntaxGroup {
        self.to_dyn_db_ast_trait().db()
    }
    fn syntax_node(&self) -> SyntaxNode {
        self.to_dyn_db_ast_trait().syntax_node()
    }
}

//////////////////////////// Typed Syntax Nodes ////////////////////////////

pub struct DbTns<'a, T> {
    _db: &'a dyn SyntaxGroup,
    pub tsn: T,
}

impl<'a, TSN: TypedSyntaxNode + Clone> DbTypedSyntaxNode<'a> for DbTns<'a, TSN> {
    type TSN = TSN;
    fn typed_syntax_node(&self) -> Self::TSN {
        self.tsn.clone()
    }
}

impl<'a, T: TypedSyntaxNode> DbSyntaxNode<'a> for DbTns<'a, T> {
    fn db(&self) -> &dyn SyntaxGroup {
        self._db
    }
    fn syntax_node(&self) -> SyntaxNode {
        self.tsn.as_syntax_node()
    }
}

impl<'a, TSN: TypedSyntaxNode> NewDbTypedSyntaxNode<'a> for DbTns<'a, TSN> {
    type TSN = TSN;
    fn new(db: &'a dyn SyntaxGroup, node: Self::TSN) -> Self {
        Self { _db: db, tsn: node }
    }
}

pub type GenericParamList<'a> = DbTns<'a, ast::OptionWrappedGenericParamList>;
pub type GenericArgList<'a> = DbTns<'a, &'a ast::GenericArgList>;

////////////////// Items //////////////////

pub fn element_list_to_vec<
    'a,
    TSN: TypedSyntaxNode + Clone,
    T: NewDbTypedSyntaxNode<'a, TSN = TSN>,
    S: Deref<Target = AstElementList<TSN, STEP>>,
    const STEP: usize,
>(
    db: &'a dyn SyntaxGroup,
    node: S,
) -> Vec<T> {
    node.elements(db)
        .iter()
        .map(|e: &TSN| T::new(db, e.clone()))
        .collect()
}

pub fn derive_attrs(db: &dyn SyntaxGroup, attr_list: ast::AttributeList) -> Vec<String> {
    attr_list
        .query_attr(db, "derive")
        .iter()
        .filter_map(|attr| {
            let args = attr.clone().structurize(db).args;
            if args.is_empty() {
                None
            } else {
                Some(args.into_iter().filter_map(|a| {
                    if let AttributeArgVariant::Unnamed(ast::Expr::Path(path)) = a.variant {
                        if let [ast::PathSegment::Simple(segment)] = &path.elements(db)[..] {
                            Some(segment.ident(db).text(db).to_string())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                }))
            }
        })
        .flatten()
        .collect::<Vec<_>>()
}

#[derive(Clone, Debug)]
pub enum Visibility {
    Default,
    Pub,
}
