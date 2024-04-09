module Test.Partest.Internal.Parser where

import Text.Gigaparsec (Parsec, (<|>))
import Text.Gigaparsec.Char (char)
import Text.Gigaparsec.Combinator (choice)
import Data.List.NonEmpty (NonEmpty ((:|)), (<|))

syntax = undefined

rule = undefined

optWhitespace = undefined

expression = undefined

lineEnd = undefined

list = undefined

term = undefined

literal = undefined

text1 = undefined

text2 = undefined

character = undefined

letter :: Parsec Char
letter = choice $ char <$> (['a' .. 'z'] ++ ['A' .. 'Z'])

digit :: Parsec Char
digit = choice $ char <$> ['0' .. '9']

symbol :: Parsec Char
symbol =
  choice $
    char
      <$> [ '|'
          , ' '
          , '!'
          , '#'
          , '$'
          , '%'
          , '&'
          , '('
          , ')'
          , '*'
          , '+'
          , ','
          , '-'
          , '.'
          , '/'
          , ':'
          , ';'
          , '>'
          , '='
          , '<'
          , '?'
          , '@'
          , '['
          , '\\'
          , ']'
          , '^'
          , '_'
          , '`'
          , '{'
          , '}'
          , '~'
          ]

character1 = character <|> char '\''

character2 = character <|> char '"'

ruleName = letter <|> ruleName

ruleChar = letter <|> digit <|> char '-'

data PTerm = PStr String | PSym String

newtype PSeqs = PSeqs (NonEmpty PTerm)

data PChoices = PChoices String (NonEmpty PSeqs)

newtype PRules = PRules (NonEmpty PChoices)

sym :: String -> PSeqs
sym x = PSeqs (PSym x :| [])

str :: String -> PSeqs
str x = PSeqs (PStr x :| [])

pType :: PChoices
pType = PChoices "type" (sym "baseType" <| sym "arrayType" <| sym "pairType" :| [])
pBaseType :: PChoices
pBaseType = PChoices "baseType" (str "int" <| str "bool" <| str "char" <| str "string" :| [])
pArrayType :: PChoices
pArrayType = PChoices "arrayType" (PSeqs (PSym "type" <| PStr "[" <| PStr "]" :| []) :| [])
pPairType :: PChoices
pPairType = PChoices "pairType" (PSeqs (PStr "pair" <| PStr "(" <| PSym "pairElemType" <| PStr "," <| PSym "pairElemType" <| PStr ")" :| []) :| [])
pPairElemType :: PChoices
pPairElemType = PChoices "pairElemType" (sym "baseType" <| sym "arrayType" <| str "pair" :| [])

pRules = PRules (pType <| pBaseType <| pArrayType <| pPairType <| pPairElemType :| [])
-- pRules = PRules (pType :| [])