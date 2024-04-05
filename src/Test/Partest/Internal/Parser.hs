module Test.Partest.Internal.Parser where

import Text.Gigaparsec (Parsec, (<|>))
import Text.Gigaparsec.Char (char)
import Text.Gigaparsec.Combinator (choice)

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