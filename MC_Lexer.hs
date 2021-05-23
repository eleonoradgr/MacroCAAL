module MC_Lexer where

import Data.Maybe
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.String
import qualified Text.Parsec.Token as Token

languageDef =
  emptyDef
    { Token.commentStart = "",
      Token.commentEnd = "",
      Token.commentLine = "*",
      Token.nestedComments = False,
      Token.identStart = lower,
      Token.identLetter = alphaNum,
      Token.reservedNames =
        [ "if",
          "then",
          "else",
          "while",
          "do",
          "skip",
          "true",
          "false",
          "not",
          "and",
          "or",
          "set",
          "def"
        ],
      Token.reservedOpNames =
        [ "+",
          "|",
          ".",
          "'",
          "\\",
          "-",
          "*",
          "/",
          ":=",
          "<",
          ">",
          "==",
          "<=",
          ">=",
          "!=",
          "++",
          "and",
          "or",
          "not",
          "&&",
          "||",
          "!"
        ],
      Token.caseSensitive = True
    }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer

procIdentifier = do
  whiteSpace
  x <- Token.lexeme lexer upper
  xs <- optionMaybe $ many (Token.lexeme lexer alphaNum)
  if isJust xs
    then return (x : fromJust xs)
    else return [x]

reserved = Token.reserved lexer

reservedOp = Token.reservedOp lexer

integer = Token.integer lexer

natural = Token.natural lexer

whiteSpace = Token.whiteSpace lexer

parens = Token.parens lexer

braces = Token.braces lexer

angles = Token.angles lexer

brackets = Token.brackets lexer

semi = Token.semi lexer

comma = Token.comma lexer

colon = Token.colon lexer

dot = Token.dot lexer
