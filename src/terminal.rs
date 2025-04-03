use cairo_lang_syntax::node::{ast, db::SyntaxGroup, SyntaxNode, Terminal, Token, TypedSyntaxNode};

use crate::{syntax_element::get_child, NodeToElement};

macro_rules! impl_node_to_element_for_all_terminals {
    ($type:ty) => {
        impl<'a> NodeToElement<'a, $type> for String
        where
            $type: Terminal,
        {
            fn node_to_element(db: &'a dyn SyntaxGroup, node: SyntaxNode) -> String {
                <$type as Terminal>::TokenType::from_syntax_node(db, get_child::<1>(db, node))
                    .text(db)
                    .to_string()
            }
        }
    };
}

impl_node_to_element_for_all_terminals!(ast::TerminalIdentifier);
impl_node_to_element_for_all_terminals!(ast::TerminalLiteralNumber);
impl_node_to_element_for_all_terminals!(ast::TerminalShortString);
impl_node_to_element_for_all_terminals!(ast::TerminalString);
impl_node_to_element_for_all_terminals!(ast::TerminalAs);
impl_node_to_element_for_all_terminals!(ast::TerminalConst);
impl_node_to_element_for_all_terminals!(ast::TerminalElse);
impl_node_to_element_for_all_terminals!(ast::TerminalEnum);
impl_node_to_element_for_all_terminals!(ast::TerminalExtern);
impl_node_to_element_for_all_terminals!(ast::TerminalFalse);
impl_node_to_element_for_all_terminals!(ast::TerminalFunction);
impl_node_to_element_for_all_terminals!(ast::TerminalIf);
impl_node_to_element_for_all_terminals!(ast::TerminalWhile);
impl_node_to_element_for_all_terminals!(ast::TerminalFor);
impl_node_to_element_for_all_terminals!(ast::TerminalLoop);
impl_node_to_element_for_all_terminals!(ast::TerminalImpl);
impl_node_to_element_for_all_terminals!(ast::TerminalImplicits);
impl_node_to_element_for_all_terminals!(ast::TerminalLet);
impl_node_to_element_for_all_terminals!(ast::TerminalMatch);
impl_node_to_element_for_all_terminals!(ast::TerminalModule);
impl_node_to_element_for_all_terminals!(ast::TerminalMut);
impl_node_to_element_for_all_terminals!(ast::TerminalNoPanic);
impl_node_to_element_for_all_terminals!(ast::TerminalOf);
impl_node_to_element_for_all_terminals!(ast::TerminalRef);
impl_node_to_element_for_all_terminals!(ast::TerminalContinue);
impl_node_to_element_for_all_terminals!(ast::TerminalReturn);
impl_node_to_element_for_all_terminals!(ast::TerminalBreak);
impl_node_to_element_for_all_terminals!(ast::TerminalStruct);
impl_node_to_element_for_all_terminals!(ast::TerminalTrait);
impl_node_to_element_for_all_terminals!(ast::TerminalTrue);
impl_node_to_element_for_all_terminals!(ast::TerminalType);
impl_node_to_element_for_all_terminals!(ast::TerminalUse);
impl_node_to_element_for_all_terminals!(ast::TerminalPub);
impl_node_to_element_for_all_terminals!(ast::TerminalAnd);
impl_node_to_element_for_all_terminals!(ast::TerminalAndAnd);
impl_node_to_element_for_all_terminals!(ast::TerminalArrow);
impl_node_to_element_for_all_terminals!(ast::TerminalAt);
impl_node_to_element_for_all_terminals!(ast::TerminalBadCharacters);
impl_node_to_element_for_all_terminals!(ast::TerminalColon);
impl_node_to_element_for_all_terminals!(ast::TerminalColonColon);
impl_node_to_element_for_all_terminals!(ast::TerminalComma);
impl_node_to_element_for_all_terminals!(ast::TerminalDiv);
impl_node_to_element_for_all_terminals!(ast::TerminalDivEq);
impl_node_to_element_for_all_terminals!(ast::TerminalDot);
impl_node_to_element_for_all_terminals!(ast::TerminalDotDot);
impl_node_to_element_for_all_terminals!(ast::TerminalEndOfFile);
impl_node_to_element_for_all_terminals!(ast::TerminalEq);
impl_node_to_element_for_all_terminals!(ast::TerminalEqEq);
impl_node_to_element_for_all_terminals!(ast::TerminalGE);
impl_node_to_element_for_all_terminals!(ast::TerminalGT);
impl_node_to_element_for_all_terminals!(ast::TerminalHash);
impl_node_to_element_for_all_terminals!(ast::TerminalLBrace);
impl_node_to_element_for_all_terminals!(ast::TerminalLBrack);
impl_node_to_element_for_all_terminals!(ast::TerminalLE);
impl_node_to_element_for_all_terminals!(ast::TerminalLParen);
impl_node_to_element_for_all_terminals!(ast::TerminalLT);
impl_node_to_element_for_all_terminals!(ast::TerminalMatchArrow);
impl_node_to_element_for_all_terminals!(ast::TerminalMinus);
impl_node_to_element_for_all_terminals!(ast::TerminalMinusEq);
impl_node_to_element_for_all_terminals!(ast::TerminalMod);
impl_node_to_element_for_all_terminals!(ast::TerminalModEq);
impl_node_to_element_for_all_terminals!(ast::TerminalMul);
impl_node_to_element_for_all_terminals!(ast::TerminalMulEq);
impl_node_to_element_for_all_terminals!(ast::TerminalNeq);
impl_node_to_element_for_all_terminals!(ast::TerminalNot);
impl_node_to_element_for_all_terminals!(ast::TerminalBitNot);
impl_node_to_element_for_all_terminals!(ast::TerminalOr);
impl_node_to_element_for_all_terminals!(ast::TerminalOrOr);
impl_node_to_element_for_all_terminals!(ast::TerminalPlus);
impl_node_to_element_for_all_terminals!(ast::TerminalPlusEq);
impl_node_to_element_for_all_terminals!(ast::TerminalQuestionMark);
impl_node_to_element_for_all_terminals!(ast::TerminalRBrace);
impl_node_to_element_for_all_terminals!(ast::TerminalRBrack);
impl_node_to_element_for_all_terminals!(ast::TerminalRParen);
impl_node_to_element_for_all_terminals!(ast::TerminalSemicolon);
impl_node_to_element_for_all_terminals!(ast::TerminalUnderscore);
impl_node_to_element_for_all_terminals!(ast::TerminalXor);
impl_node_to_element_for_all_terminals!(ast::TerminalEmpty);
