use cairo_lang_defs::patcher::PatchBuilder;
use cairo_lang_filesystem::span::{TextOffset, TextPosition, TextSpan, TextWidth};
use cairo_lang_syntax::node::db::SyntaxGroup;
use cairo_lang_syntax::node::green::GreenNode;
use cairo_lang_syntax::node::kind::SyntaxKind;
use cairo_lang_syntax::node::{ast, SyntaxNode, Terminal, TypedSyntaxNode};
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

// pub trait ElementList<'a, const STEP: usize, TSN, E: NodeToElement<'a, TSN>> {
//     const STEP: usize = STEP;

//     fn elements(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Vec<E> {
//         db.get_children(node)
//             .iter()
//             .step_by(STEP)
//             .map(|sn| E::node_to_element(db, sn.clone()))
//             .collect()
//     }
// }

pub trait ElementList {
    const STEP: usize;
    type TSN: TypedSyntaxNode;
}

// impl<'a, const STEP: usize, TSN, E: NodeToElement<'a, TSN>> ElementList<'a, E> for TSN {
//     const STEP: usize = STEP;
//     fn elements(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Vec<E> {
//         db.get_children(node)
//             .iter()
//             .step_by(STEP)
//             .map(|sn| E::node_to_element(db, sn.clone()))
//             .collect()
//     }
// }

// pub trait NodeToElement<'a, TSN: TypedSyntaxNode, E> {
//     fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> E;
//     fn child_node_to_element<const INDEX: usize>(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> E
//     where
//         Self: Sized,
//     {
//         Self::node_to_element(db, get_child::<INDEX>(db, node))
//     }
// }

pub trait NodeToElement<'a, TSN> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self;
    fn child_node_to_element<const INDEX: usize>(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self
    where
        Self: Sized,
    {
        Self::node_to_element(db, get_child::<INDEX>(db, node))
    }
}

impl<'a, EL: ElementList, E> NodeToElement<'a, EL> for Vec<E>
where
    E: NodeToElement<'a, EL::TSN>,
{
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Vec<E> {
        db.get_children(node)
            .iter()
            .step_by(EL::STEP)
            .map(|sn| E::node_to_element(db, sn.clone()))
            .collect()
    }
}

// impl<'a, TSN, const STEP: usize, E> ElementList<'a, TSN, STEP, E> for Vec<E>
// where
//     TSN: TypedSyntaxNode,
//     E: NodeToElement<'a, TSN>,
// {
//     fn elements(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self {
//         db.get_children(node)
//             .iter()
//             .step_by(STEP)
//             .map(|sn| E::node_to_element(db, sn.clone()))
//             .collect()
//     }
// }

// pub trait NodeToElement<'a, TSN, E> {
//     fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> E;
//     fn child_node_to_element<const INDEX: usize>(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> E
//     where
//         Self: Sized,
//     {
//         Self::node_to_element(db, get_child::<INDEX>(db, node))
//     }
// }

// impl<TSN, TypedSyntaxElement> NodeToElement<'_, TypedSyntaxElement<TSN>> for TSN {
//     fn node_to_element(db: &dyn SyntaxGroup, node: SyntaxNode) -> TypedSyntaxElement<TSN> {
//         TypedSyntaxElement::from_syntax_node(db, node)
//     }
// }

// pub trait ElementFromNode<'a, TSN> {
//     fn element_from_node(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self;
//     fn child_node_to_element<const INDEX: usize>(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self
//     where
//         Self: Sized,
//     {
//         Self::node_to_element(db, get_child::<INDEX>(db, node))
//     }
// }
impl<'a, TSN: TypedSyntaxNode> NodeToElement<'a, TSN> for TypedSyntaxElement<'a, TSN> {
    fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> TypedSyntaxElement<'a, TSN> {
        TypedSyntaxElement::<'a, TSN>::from_syntax_node(db, node)
    }
}

pub trait SyntaxElementTrait<'a> {
    fn from_syntax_node(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> Self;
    fn get_db(&self) -> &'a dyn SyntaxGroup;
    fn get_syntax_node(&self) -> SyntaxNode;
    fn to_syntax_node(self) -> SyntaxNode;

    fn get_children(&self) -> Arc<[SyntaxNode]>;
    fn patch_builder(&self) -> PatchBuilder;
    fn get_child<const INDEX: usize>(&self) -> SyntaxNode;
    fn get_child_kind<const INDEX: usize>(&self) -> SyntaxKind;
    fn get_child_element<const INDEX: usize, TSN, E: NodeToElement<'a, TSN>>(&self) -> E {
        E::node_to_element(self.get_db(), self.get_child::<INDEX>())
    }
    fn get_child_syntax_element<const INDEX: usize>(&self) -> SyntaxElement<'a> {
        SyntaxElement::from_syntax_node(self.get_db(), self.get_child::<INDEX>())
    }
    fn as_element<TSN, E>(&self) -> E
    where
        E: NodeToElement<'a, TSN>,
    {
        E::node_to_element(self.get_db(), self.get_syntax_node())
    }
    fn to_element<TSN, E: NodeToElement<'a, TSN>>(self) -> E
    where
        Self: Sized,
    {
        NodeToElement::node_to_element(self.get_db(), self.to_syntax_node())
    }
    fn from_typed_syntax_node<TSN: TypedSyntaxNode>(db: &'a dyn SyntaxGroup, node: TSN) -> Self
    where
        Self: Sized,
    {
        Self::from_syntax_node(db, node.as_syntax_node())
    }
    fn as_typed_syntax_node<TSN: TypedSyntaxNode>(&self) -> TSN {
        TSN::from_syntax_node(self.get_db(), self.get_syntax_node())
    }
    fn get_child_typed_syntax_node<const INDEX: usize, TSN: TypedSyntaxNode>(&self) -> TSN {
        TSN::from_syntax_node(self.get_db(), self.get_child::<INDEX>())
    }
    fn offset(&self) -> TextOffset {
        self.get_syntax_node().offset()
    }
    fn width(&self) -> TextWidth {
        self.get_syntax_node().width(self.get_db())
    }
    fn kind(&self) -> SyntaxKind {
        self.get_syntax_node().kind(self.get_db())
    }
    fn span(&self) -> TextSpan {
        self.get_syntax_node().span(self.get_db())
    }
    fn text(&self) -> Option<SmolStr> {
        self.get_syntax_node().text(self.get_db())
    }
    fn green_node(&self) -> Arc<GreenNode> {
        self.get_syntax_node().green_node(self.get_db())
    }
    fn span_without_trivia(&self) -> TextSpan {
        self.get_syntax_node().span_without_trivia(self.get_db())
    }
    fn position_in_parent(&self) -> Option<usize> {
        self.get_syntax_node().position_in_parent(self.get_db())
    }
    fn get_terminal_token(&self) -> Option<SyntaxNode> {
        self.get_syntax_node().get_terminal_token(self.get_db())
    }
    fn span_start_without_trivia(&self) -> TextOffset {
        self.get_syntax_node()
            .span_start_without_trivia(self.get_db())
    }
    fn span_end_without_trivia(&self) -> TextOffset {
        self.get_syntax_node()
            .span_end_without_trivia(self.get_db())
    }
    fn lookup_offset(&self, offset: TextOffset) -> SyntaxNode {
        self.get_syntax_node().lookup_offset(self.get_db(), offset)
    }
    fn lookup_position(&self, position: TextPosition) -> SyntaxNode {
        self.get_syntax_node()
            .lookup_position(self.get_db(), position)
    }
    fn get_text(&self) -> String {
        self.get_syntax_node().get_text(self.get_db())
    }
    fn get_text_without_inner_commentable_children(&self) -> String {
        self.get_syntax_node()
            .get_text_without_inner_commentable_children(self.get_db())
    }
    fn get_text_without_all_comment_trivia(&self) -> String {
        self.get_syntax_node()
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
        self.get_children()[INDEX].clone()
    }
    fn get_child_kind<const INDEX: usize>(&self) -> SyntaxKind {
        self.get_child::<INDEX>().kind(self.db)
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
        self.node.clone()
    }
    fn to_syntax_node(self) -> SyntaxNode {
        self.node
    }
    fn get_children(&self) -> Arc<[SyntaxNode]> {
        self.children.clone()
    }
    fn get_child<const INDEX: usize>(&self) -> SyntaxNode {
        self.children[INDEX].clone()
    }
    fn get_child_kind<const INDEX: usize>(&self) -> SyntaxKind {
        self.children[INDEX].kind(self.db)
    }
    fn patch_builder(&self) -> PatchBuilder {
        PatchBuilder::new_ex(self.db, &self.node)
    }
}

impl<'a> SyntaxElement<'a> {
    pub fn as_typed_syntax_element<TNS: TypedSyntaxNode>(&self) -> TypedSyntaxElement<'a, TNS> {
        TypedSyntaxElement::from_syntax_node(self.db, self.node.clone())
    }
    pub fn to_typed_syntax_element<TNS: TypedSyntaxNode>(self) -> TypedSyntaxElement<'a, TNS> {
        TypedSyntaxElement::from_syntax_node(self.db, self.node)
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
