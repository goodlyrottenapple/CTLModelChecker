module Parser where 

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import CTLModelChecker

def :: LanguageDef st
def = emptyDef{ commentStart = "{*"
              , commentEnd = "*}"
              , identStart = letter
              , identLetter = alphaNum
              , opStart = oneOf "{}[]/\\-t?!UO"
              , opLetter = oneOf "{}[]/\\-t?!UO"
              , reservedOpNames = ["/\\", "\\/", "?{}", "!{}", "?[]", "![]", "?U", "!U", "?O", "!O","true", "-"]
              }


TokenParser{ parens = m_parens
           , identifier = m_identifier
           , reservedOp = m_reservedOp
           , reserved = m_reserved
           , semiSep1 = m_semiSep1
           , whiteSpace = m_whiteSpace } = makeTokenParser def


orF :: Formula -> Formula -> Formula
orF x y = Not (And (Not x) (Not y))

eDiam :: Formula -> Formula
eDiam x = EUntil T x

aDiam :: Formula -> Formula
aDiam x = AUntil T x

eBox :: Formula -> Formula
eBox x = Not (aDiam (Not x))

aBox :: Formula -> Formula
aBox x = Not (eDiam (Not x))


formparser :: Parser Formula
formparser = buildExpressionParser formparser_table formparser_term <?> "formula"
formparser_table = [ 
    [Prefix (m_reservedOp "-" >> return Not)],
    [Infix (m_reservedOp "/\\" >> return And) AssocLeft],
    [Infix (m_reservedOp "\\/" >> return orF) AssocLeft],
    [Infix (m_reservedOp "?U" >> return EUntil) AssocLeft],
    [Infix (m_reservedOp "!U" >> return AUntil) AssocLeft],
    [Prefix (m_reservedOp "?O" >> return ENext)],
    [Prefix (m_reservedOp "!O" >> return ANext)],
    [Prefix (m_reservedOp "?{}" >> return eDiam)],
    [Prefix (m_reservedOp "!{}" >> return aDiam)],
    [Prefix (m_reservedOp "?[]" >> return eBox)],
    [Prefix (m_reservedOp "![]" >> return aBox)]
  ]

formparser_term = 
  try(m_parens formparser)
  <|> do { x <- m_identifier ; return (A x) }


parseF :: String -> Formula
parseF inp = case parse (m_whiteSpace >> formparser) "" inp of
             { Left err -> A "Error"
             ; Right ans -> ans
         }
