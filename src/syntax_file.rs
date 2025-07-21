use crate::item::Item;
use crate::syntax_element::SyntaxElementTrait;
use crate::{ElementList, NodeToElement, TypedSyntaxElement};
use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_macro::TokenStream;
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_syntax::node::ast;

pub type SyntaxFile<'a> = TypedSyntaxElement<'a, ast::SyntaxFile>;
pub type TerminalEndOfFile<'a> = TypedSyntaxElement<'a, ast::TerminalEndOfFile>;

impl ElementList for ast::Trivia {
    const STEP: usize = 1;
    type TSN = ast::Trivium;
}

impl<'a> SyntaxFile<'a> {
    pub const INDEX_ITEMS: usize = ast::SyntaxFile::INDEX_ITEMS;
    pub const INDEX_EOF: usize = ast::SyntaxFile::INDEX_EOF;
    pub fn items(&self) -> Vec<Item> {
        self.get_child_element::<{ SyntaxFile::INDEX_ITEMS }, ast::ModuleItemList, Vec<Item>>()
    }
    pub fn item(&self) -> Item {
        self.items().iter().next().unwrap().clone()
    }
    pub fn eof<T>(&self) -> T where T: NodeToElement<'a, ast::TerminalEndOfFile> {
        self.get_child_element::<{ SyntaxFile::INDEX_EOF }, ast::TerminalEndOfFile, _>()
    }
}

pub fn parse_token_stream_to_syntax_file(
    token_stream: TokenStream,
) -> (SyntaxFile<'static>, Diagnostics<ParserDiagnostic>) {
    let db = Box::leak(Box::new(SimpleParserDatabase::default()));
    let (parsed, diagnostics) = db.parse_virtual_with_diagnostics(token_stream);
    (SyntaxFile::from_syntax_node(db, parsed), diagnostics)
}

impl <'a> TerminalEndOfFile<'a> {
    pub const INDEX_LEADING_TRIVIA: usize = 0;
    pub const INDEX_TOKEN: usize = 1;
    pub const INDEX_TRAILING_TRIVIA: usize = 2;
    pub fn leading_trivia<T>(&self) -> T where T: NodeToElement<'a, ast::Trivia> {
        self.get_child_element::<{ TerminalEndOfFile::INDEX_LEADING_TRIVIA }, ast::Trivia, _>()
    }
    pub fn token(&self) -> ast::TokenEndOfFile {
        self.get_child_element::<{ TerminalEndOfFile::INDEX_TOKEN }, ast::TokenEndOfFile, _>()
    }
    pub fn trailing_trivia<T>(&self) -> T where T: NodeToElement<'a, ast::Trivia> {
        self.get_child_element::<{ TerminalEndOfFile::INDEX_TRAILING_TRIVIA }, ast::Trivia, _>()
    }

}


