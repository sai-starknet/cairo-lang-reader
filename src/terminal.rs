use cairo_lang_syntax::node::{ast, db::SyntaxGroup, SyntaxNode, Terminal, Token, TypedSyntaxNode};

use crate::{syntax_element::get_child, NodeToElement};

macro_rules! impl_node_to_element_for_terminal {
    ($type:ty) => {
        impl<'a> NodeToElement<'a, $type> for String {
            fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> String {
                <$type as Terminal>::TokenType::from_syntax_node(db, get_child::<1>(db, node))
                    .text(db)
                    .to_string()
            }
        }
    };
}

impl_node_to_element_for_terminal!(ast::TerminalIdentifier);
impl_node_to_element_for_terminal!(ast::TerminalLiteralNumber);
impl_node_to_element_for_terminal!(ast::TerminalShortString);
impl_node_to_element_for_terminal!(ast::TerminalString);
impl_node_to_element_for_terminal!(ast::TerminalAs);
impl_node_to_element_for_terminal!(ast::TerminalConst);
impl_node_to_element_for_terminal!(ast::TerminalElse);
impl_node_to_element_for_terminal!(ast::TerminalEnum);
impl_node_to_element_for_terminal!(ast::TerminalExtern);
impl_node_to_element_for_terminal!(ast::TerminalFalse);
impl_node_to_element_for_terminal!(ast::TerminalFunction);
impl_node_to_element_for_terminal!(ast::TerminalIf);
impl_node_to_element_for_terminal!(ast::TerminalWhile);
impl_node_to_element_for_terminal!(ast::TerminalFor);
impl_node_to_element_for_terminal!(ast::TerminalLoop);
impl_node_to_element_for_terminal!(ast::TerminalImpl);
impl_node_to_element_for_terminal!(ast::TerminalImplicits);
impl_node_to_element_for_terminal!(ast::TerminalLet);
impl_node_to_element_for_terminal!(ast::TerminalMatch);
impl_node_to_element_for_terminal!(ast::TerminalModule);
impl_node_to_element_for_terminal!(ast::TerminalMut);
impl_node_to_element_for_terminal!(ast::TerminalNoPanic);
impl_node_to_element_for_terminal!(ast::TerminalOf);
impl_node_to_element_for_terminal!(ast::TerminalRef);
impl_node_to_element_for_terminal!(ast::TerminalContinue);
impl_node_to_element_for_terminal!(ast::TerminalReturn);
impl_node_to_element_for_terminal!(ast::TerminalBreak);
impl_node_to_element_for_terminal!(ast::TerminalStruct);
impl_node_to_element_for_terminal!(ast::TerminalTrait);
impl_node_to_element_for_terminal!(ast::TerminalTrue);
impl_node_to_element_for_terminal!(ast::TerminalType);
impl_node_to_element_for_terminal!(ast::TerminalUse);
impl_node_to_element_for_terminal!(ast::TerminalPub);
impl_node_to_element_for_terminal!(ast::TerminalAnd);
impl_node_to_element_for_terminal!(ast::TerminalAndAnd);
impl_node_to_element_for_terminal!(ast::TerminalArrow);
impl_node_to_element_for_terminal!(ast::TerminalAt);
impl_node_to_element_for_terminal!(ast::TerminalBadCharacters);
impl_node_to_element_for_terminal!(ast::TerminalColon);
impl_node_to_element_for_terminal!(ast::TerminalColonColon);
impl_node_to_element_for_terminal!(ast::TerminalComma);
impl_node_to_element_for_terminal!(ast::TerminalDiv);
impl_node_to_element_for_terminal!(ast::TerminalDivEq);
impl_node_to_element_for_terminal!(ast::TerminalDot);
impl_node_to_element_for_terminal!(ast::TerminalDotDot);
impl_node_to_element_for_terminal!(ast::TerminalEndOfFile);
impl_node_to_element_for_terminal!(ast::TerminalEq);
impl_node_to_element_for_terminal!(ast::TerminalEqEq);
impl_node_to_element_for_terminal!(ast::TerminalGE);
impl_node_to_element_for_terminal!(ast::TerminalGT);
impl_node_to_element_for_terminal!(ast::TerminalHash);
impl_node_to_element_for_terminal!(ast::TerminalLBrace);
impl_node_to_element_for_terminal!(ast::TerminalLBrack);
impl_node_to_element_for_terminal!(ast::TerminalLE);
impl_node_to_element_for_terminal!(ast::TerminalLParen);
impl_node_to_element_for_terminal!(ast::TerminalLT);
impl_node_to_element_for_terminal!(ast::TerminalMatchArrow);
impl_node_to_element_for_terminal!(ast::TerminalMinus);
impl_node_to_element_for_terminal!(ast::TerminalMinusEq);
impl_node_to_element_for_terminal!(ast::TerminalMod);
impl_node_to_element_for_terminal!(ast::TerminalModEq);
impl_node_to_element_for_terminal!(ast::TerminalMul);
impl_node_to_element_for_terminal!(ast::TerminalMulEq);
impl_node_to_element_for_terminal!(ast::TerminalNeq);
impl_node_to_element_for_terminal!(ast::TerminalNot);
impl_node_to_element_for_terminal!(ast::TerminalBitNot);
impl_node_to_element_for_terminal!(ast::TerminalOr);
impl_node_to_element_for_terminal!(ast::TerminalOrOr);
impl_node_to_element_for_terminal!(ast::TerminalPlus);
impl_node_to_element_for_terminal!(ast::TerminalPlusEq);
impl_node_to_element_for_terminal!(ast::TerminalQuestionMark);
impl_node_to_element_for_terminal!(ast::TerminalRBrace);
impl_node_to_element_for_terminal!(ast::TerminalRBrack);
impl_node_to_element_for_terminal!(ast::TerminalRParen);
impl_node_to_element_for_terminal!(ast::TerminalSemicolon);
impl_node_to_element_for_terminal!(ast::TerminalUnderscore);
impl_node_to_element_for_terminal!(ast::TerminalXor);
impl_node_to_element_for_terminal!(ast::TerminalEmpty);
