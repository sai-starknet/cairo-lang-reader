use crate::db_tns::NewDbTypedSyntaxNode;
use crate::item::Item;
use crate::syntax_element::SyntaxElementTrait;
use crate::{element_list_to_vec, TypedSyntaxElement};
use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_macro::TokenStream;
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_syntax::node::ast;

pub type SyntaxFile<'a> = TypedSyntaxElement<'a, ast::SyntaxFile>;

impl SyntaxFile<'_> {
    const ITEMS_INDEX: usize = 0;
    const EOF_INDEX: usize = 1;
    pub fn items(&self) -> Vec<Item> {
        self.get_child_vec::<Self::ITEMS_INDEX>()
    }
    pub fn item(&self) -> Item {
        Item::new(
            self.db,
            self.as_typed_syntax_node()
                .items(self.db)
                .elements(self.db)
                .iter()
                .next()
                .unwrap()
                .clone(),
        )
    }
}

pub fn parse_token_stream_to_syntax_file(
    token_stream: TokenStream,
) -> (SyntaxFile<'static>, Diagnostics<ParserDiagnostic>) {
    let db = Box::leak(Box::new(SimpleParserDatabase::default()));
    let (parsed, diagnostics) = db.parse_virtual_with_diagnostics(token_stream);
    (SyntaxFile::from_syntax_node(db, parsed), diagnostics)
}
