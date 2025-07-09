use crate::item::Item;
use crate::syntax_element::SyntaxElementTrait;
use crate::TypedSyntaxElement;
use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_macro::TokenStream;
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_syntax::node::ast;

pub type SyntaxFile<'a> = TypedSyntaxElement<'a, ast::SyntaxFile>;

impl SyntaxFile<'_> {
    pub const ITEMS_INDEX: usize = ast::SyntaxFile::INDEX_ITEMS;
    pub const EOF_INDEX: usize = ast::SyntaxFile::INDEX_EOF;
    pub fn items(&self) -> Vec<Item> {
        self.get_child_element::<{ SyntaxFile::ITEMS_INDEX }, ast::ModuleItemList, Vec<Item>>()
    }
    pub fn item(&self) -> Item {
        self.items().iter().next().unwrap().clone()
    }
}

pub fn parse_token_stream_to_syntax_file(
    token_stream: TokenStream,
) -> (SyntaxFile<'static>, Diagnostics<ParserDiagnostic>) {
    let db = Box::leak(Box::new(SimpleParserDatabase::default()));
    let (parsed, diagnostics) = db.parse_virtual_with_diagnostics(token_stream);
    (SyntaxFile::from_syntax_node(db, parsed), diagnostics)
}
