use crate::db_tns::NewDbTypedSyntaxNode;
use crate::item::Item;
use crate::{element_list_to_vec, DbSyntaxNode, DbTns};
use cairo_lang_diagnostics::Diagnostics;
use cairo_lang_macro::TokenStream;
use cairo_lang_parser::utils::SimpleParserDatabase;
use cairo_lang_parser::ParserDiagnostic;
use cairo_lang_syntax::node::{ast, TypedSyntaxNode};

pub type SyntaxFile<'a> = DbTns<'a, ast::SyntaxFile>;

impl SyntaxFile<'_> {
    pub fn items(&self) -> Vec<Item> {
        element_list_to_vec(self.db(), self.tsn.items(self.db()))
    }
    pub fn item(&self) -> Item {
        Item::new(
            self.db(),
            self.tsn
                .items(self.db())
                .elements(self.db())
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
    let syntax_file = ast::SyntaxFile::from_syntax_node(db, parsed);
    (SyntaxFile::new(db, syntax_file), diagnostics)
}
