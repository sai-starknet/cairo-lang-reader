use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_filesystem::span::{TextOffset, TextPosition, TextSpan, TextWidth};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::green::GreenNode;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{SyntaxNode, TypedSyntaxNode};
use smol_str::SmolStr;
use std::marker::PhantomData;
use std::sync::Arc;

#[derive(Clone)]
pub struct SyntaxElement<'a> {
    pub db: &'a dyn SyntaxGroup,
    pub node: SyntaxNode,
}

#[derive(Clone)]
pub struct TypedSyntaxElement<'a, TSN: TypedSyntaxNode> {
    pub db: &'a dyn SyntaxGroup,
    pub node: SyntaxNode,
    phantom: PhantomData<TSN>,
    pub children: Arc<[SyntaxNode]>,
}

pub fn get_child<const INDEX: usize>(db: &dyn SyntaxGroup, node: SyntaxNode) -> SyntaxNode {
    db.get_children(node).get(INDEX).unwrap().clone()
}

pub fn syntax_node_to_vec<
    'a,
    const STEP: usize,
    TSN: TypedSyntaxNode,
    T: NodeToElement<'a, TSN>,
>(
    db: &'a dyn SyntaxGroup,
    node: SyntaxNode,
) -> Vec<T> {
    db.get_children(node)
        .iter()
        .step_by(STEP)
        .map(|sn| T::node_to_element(db, sn))
        .collect()
}

pub fn child_syntax_node_to_vec<
    'a,
    const INDEX: usize,
    const STEP: usize,
    TSN: TypedSyntaxNode,
    T: NodeToElement<'a, TSN>,
>(
    db: &'a dyn SyntaxGroup,
    node: SyntaxNode,
) -> Vec<T> {
    syntax_node_to_vec::<STEP, T>(db, get_child::<INDEX>(db, node))
}

// pub trait ToTypedSyntaxElementLike<'a> {
//     fn to_typed_syntax_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self;
//     fn to_child_typed_syntax_element<const INDEX: usize>(
//         db: &'a dyn SyntaxGroup,
//         node: SyntaxNode,
//     ) -> Self
//     where
//         Self: Sized,
//     {
//         Self::to_typed_syntax_element(db, get_child::<INDEX>(db, node))
//     }
// }

pub trait NodeToElement<'a, TSN> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self;
    fn child_node_to_element<const INDEX: usize>(
        db: &'a dyn SyntaxGroup,
        node: SyntaxNode,
    ) -> Self {
        Self::node_to_element(db, get_child::<INDEX>(db, node))
    }
}

pub trait CreateElement<'a> {
    fn create_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self;
}

impl<'a, T> CreateElement<'a> for T
where
    T: SyntaxElementTrait<'a>,
{
    fn create_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
        T::from_syntax_node(db, node)
    }
}

impl<'a, TSN: TypedSyntaxNode> NodeToElement<'a, TSN> for TypedSyntaxElement<'a, TSN> {
    fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> Self {
        Self::from_syntax_node(db, node)
    }
}

// impl<'a, TSN: TypedSyntaxNode> ToTypedSyntaxElementLike<'a> for TypedSyntaxElement<'a, TSN> {
//     fn to_typed_syntax_element(
//         db: &'a dyn SyntaxGroup,
//         node: SyntaxNode,
//     ) -> TypedSyntaxElement<'a, TSN> {
//         SyntaxElementTrait::from_syntax_node(db, node)
//     }
// }

pub trait SyntaxElementTrait<'a> {
    fn from_syntax_node(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self;
    fn get_db(&self) -> &'a dyn SyntaxGroup;
    fn get_syntax_node(&self) -> SyntaxNode;
    fn to_syntax_node(self) -> SyntaxNode;

    fn get_children(&self) -> Arc<[SyntaxNode]>;
    fn patch_builder(&self) -> PatchBuilder;
    fn get_child<const INDEX: usize>(&self) -> SyntaxNode;
    fn get_child_kind<const INDEX: usize>(&self) -> SyntaxKind;
    fn get_child_element<const INDEX: usize, TSN: TypedSyntaxNode, T: NodeToElement<'a, TSN>>(
        &self,
    );
    fn as_element(&self) -> impl CreateElement<'a> {
        CreateElement::create_element(self.get_db(), self.get_syntax_node())
    }
    fn to_element(self) -> impl CreateElement<'a> {
        CreateElement::create_element(self.get_db(), self.to_syntax_node())
    }
    fn from_typed_syntax_node<TSN: TypedSyntaxNode>(db: &'a dyn SyntaxGroup, node: TSN) -> Self
    where
        Self: Sized,
    {
        Self::from_syntax_node(db, node.as_syntax_node())
    }
    fn as_typed_syntax_node<TSN: TypedSyntaxNode>(self) -> TSN {
        TSN::from_syntax_node(self.get_db(), self.as_syntax_node())
    }
    fn get_child_typed_syntax_node<const INDEX: usize, TSN: TypedSyntaxNode>(&self) -> TSN {
        TSN::from_syntax_node(self.get_db(), self.get_child_syntax_node::<INDEX>())
    }

    fn get_child_vec<const INDEX: usize, const STEP: usize, TSN, TSE: NodeToElement<'a, TSN>>(
        &self,
    ) -> Vec<TSE> {
        syntax_node_to_vec::<STEP, TSN>(self.get_db(), self.get_child_syntax_node::<INDEX>())
    }
    fn get_grand_child_vec<
        const INDEX: usize,
        const G_INDEX: usize,
        const STEP: usize,
        TSN,
        TSE: NodeToElement<'a, TSN>,
    >(
        &self,
    ) -> Vec<TSE> {
        child_syntax_node_to_vec::<G_INDEX, STEP, TSN, TSE>(
            self.get_db(),
            self.get_child::<INDEX>(),
        )
    }
    fn offset(&self) -> TextOffset {
        self.get_node().offset()
    }
    fn width(&self) -> TextWidth {
        self.get_node().width(self.get_db())
    }
    fn kind(&self) -> SyntaxKind {
        self.get_node().kind(self.get_db())
    }
    fn span(&self) -> TextSpan {
        self.get_node().span(self.get_db())
    }
    fn text(&self) -> Option<SmolStr> {
        self.get_node().text(self.get_db())
    }
    fn green_node(&self) -> Arc<GreenNode> {
        self.get_node().green_node(self.get_db())
    }
    fn span_without_trivia(&self) -> TextSpan {
        self.get_node().span_without_trivia(self.get_db())
    }
    fn position_in_parent(&self) -> Option<usize> {
        self.get_node().position_in_parent(self.get_db())
    }
    fn get_terminal_token(&self) -> Option<SyntaxNode> {
        self.get_node().get_terminal_token(self.get_db())
    }
    fn span_start_without_trivia(&self) -> TextOffset {
        self.get_node().span_start_without_trivia(self.get_db())
    }
    fn span_end_without_trivia(&self) -> TextOffset {
        self.get_node().span_end_without_trivia(self.get_db())
    }
    fn lookup_offset(&self, offset: TextOffset) -> SyntaxNode {
        self.get_node().lookup_offset(self.get_db(), offset)
    }
    fn lookup_position(&self, position: TextPosition) -> SyntaxNode {
        self.get_node().lookup_position(self.get_db(), position)
    }
    fn get_text(&self) -> String {
        self.get_node().get_text(self.get_db())
    }
    fn get_text_without_inner_commentable_children(&self) -> String {
        self.get_node()
            .get_text_without_inner_commentable_children(self.get_db())
    }
    fn get_text_without_all_comment_trivia(&self) -> String {
        self.get_node()
            .get_text_without_all_comment_trivia(self.get_db())
    }
    fn get_text_without_trivia(self) -> String
    where
        Self: Sized,
    {
        let db = self.get_db();
        self.to_syntax_node().get_text_without_trivia(db)
    }
    fn get_text_of_span(self, span: TextSpan) -> String
    where
        Self: Sized,
    {
        let db = self.get_db();
        self.to_syntax_node().get_text_of_span(db, span)
    }
}

impl<'a> SyntaxElementTrait<'a> for SyntaxElement<'a> {
    fn from_syntax_node(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        Self { db, node }
    }
    fn get_db(&self) -> &'a dyn SyntaxGroup {
        self.db
    }
    fn get_syntax_node(&self) -> SyntaxNode {
        self.node.clone()
    }
    fn to_syntax_node(self) -> SyntaxNode {
        self.node
    }
    fn get_children(&self) -> Arc<[SyntaxNode]> {
        self.db.get_children(self.node.clone())
    }
    fn get_child<const INDEX: usize>(&self) -> SyntaxNode {
        self.get_children()[INDEX]
    }

    fn get_child_kind<const INDEX: usize>(&self) -> SyntaxKind {
        self.get_child_syntax_node::<INDEX>().kind(self.db)
    }
    fn get_child_element<const INDEX: usize, TSN: TypedSyntaxNode, T: NodeToElement<'a, TSN>>(
        &self,
    ) {
        T::node_to_element(self.db, self.get_child_syntax_node::<INDEX>())
    }
    fn patch_builder(&self) -> PatchBuilder {
        PatchBuilder::new_ex(self.db, &self.node)
    }
}

impl<'a, TSN: TypedSyntaxNode> SyntaxElementTrait<'a> for TypedSyntaxElement<'a, TSN> {
    fn from_syntax_node(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
        Self {
            children: db.get_children(node.clone()),
            db,
            node,
            phantom: PhantomData,
        }
    }
    fn get_db(&self) -> &'a dyn SyntaxGroup {
        self.db
    }
    fn get_syntax_node(&self) -> SyntaxNode {
        self.node
    }
    fn to_syntax_node(self) -> SyntaxNode {
        self.node
    }
    fn get_children(&self) -> Arc<[SyntaxNode]> {
        self.children
    }
    fn get_child<const INDEX: usize>(&self) -> SyntaxNode {
        self.children[INDEX]
    }
    fn get_child_kind<const INDEX: usize>(&self) -> SyntaxKind {
        self.children[INDEX].kind(self.db)
    }
    fn patch_builder(&self) -> PatchBuilder {
        PatchBuilder::new_ex(self.db, &self.node)
    }
    fn get_child_element<const INDEX: usize, CTSN: TypedSyntaxNode, T: NodeToElement<'a, CTSN>>(
        &self,
    ) {
        T::node_to_element(self.db, self.get_child::<INDEX>())
    }
}

impl<'a> SyntaxElement<'a> {
    pub fn as_typed_syntax_element<TNS: TypedSyntaxNode>(&self) -> TypedSyntaxElement<'a, TNS> {
        TypedSyntaxElement::from_syntax_node(self.db, self.node.clone())
    }
    pub fn to_typed_syntax_element<TNS: TypedSyntaxNode>(self) -> TypedSyntaxElement<'a, TNS> {
        TypedSyntaxElement::from_syntax_node(self.db, self.node)
    }
    fn get_child<const INDEX: usize>(&self) -> &SyntaxNode {
        self.get_children().get(INDEX).unwrap()
    }
}

impl<'a, TSN: TypedSyntaxNode> TypedSyntaxElement<'a, TSN> {
    pub fn as_syntax_element(&self) -> SyntaxElement<'a> {
        SyntaxElement::from_syntax_node(self.db, self.node.clone())
    }
    pub fn to_syntax_element(self) -> SyntaxElement<'a> {
        SyntaxElement::from_syntax_node(self.db, self.node)
    }
}

// pub fn element_list_to_vec<
//     'a,
//     T: ToTypedSyntaxElementLike<'a>,
//     S: Deref<Target = ElementList<TSN, STEP>>,
//     const STEP: usize,
//     TSN: TypedSyntaxNode,
// >(
//     db: &'a dyn SyntaxGroup,
//     list: S,
// ) -> Vec<T> {
//     db.get_children(list.node.clone())
//         .iter()
//         .step_by(STEP)
//         .map(|sn| T::to_typed_syntax_element(db, sn.clone()))
//         .collect()
// }
