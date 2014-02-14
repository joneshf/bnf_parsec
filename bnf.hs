{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module BNF where

import Control.Applicative ((<*>), (<$>), (<*))
import Data.Generics
import Data.List.NonEmpty
import Text.Parsec
--import Text.Parsec.Extra

data BNF = BNF (NonEmpty BNFProduction)
    deriving (Eq, Show, Typeable, Data)

data BNFProduction = BNFProduction String (NonEmpty BNFExpr)
    deriving (Eq, Show, Typeable, Data)

data BNFExpr = BNFExpr (NonEmpty BNFBase)
    deriving (Eq, Show, Typeable, Data)

data BNFBase = NonTerminal String
             | Terminal String
    deriving (Eq, Show, Typeable, Data)

bnf :: Stream s m Char => ParsecT s u m BNF
bnf = BNF <$> parsecMap fromList (spaces >> production `sepBy1` newline)

production :: Stream s m Char => ParsecT s u m BNFProduction
production = BNFProduction <$> anyBetweenAngle <*
                               (spaces >> string "::=" >> spaces)
                           <*> parsecMap fromList
                               (bnfExpr `sepBy1` (spaces >> char '|' >> spaces))

bnfExpr :: Stream s m Char => ParsecT s u m BNFExpr
bnfExpr = BNFExpr <$> parsecMap fromList
                      (manyTill base (lookAhead (try (base >> string "::="))))

base :: Stream s m Char => ParsecT s u m BNFBase
base = (nonTerminal <|> terminal) <* spaces

nonTerminal :: Stream s m Char => ParsecT s u m BNFBase
nonTerminal = NonTerminal <$> anyBetweenAngle

terminal :: Stream s m Char => ParsecT s u m BNFBase
terminal = Terminal <$> anyBetweenDoubleQ

-- Helpers

anyBetweenAngle :: Stream s m Char => ParsecT s u m String
anyBetweenAngle = anyBetween (char '<') (char '>')

anyBetweenDoubleQ :: Stream s m Char => ParsecT s u m String
anyBetweenDoubleQ = anyBetween (char '"') (char '"')

anyBetween :: Stream s m Char
           => ParsecT s u m open
           -> ParsecT s u m close
           -> ParsecT s u m String
anyBetween open close = open >> manyTill anyChar (try close)

-- Examples

postalAddress :: String
postalAddress = Prelude.unlines
    [ "<postal-address> ::= <name-part> <street-address> <zip-part>"
    , "<name-part> ::= <personal-part> <last-name> <opt-suffix-part> <EOL>"
    , "            | <personal-part> <name-part>"
    , "<personal-part> ::= <first-name> | <initial> \".\""
    , "<street-address> ::= <house-num> <street-name> <opt-apt-num> <EOL>"
    , "<zip-part> ::= <town-name> \",\" <state-code> <ZIP-code> <EOL>"
    , "<opt-suffix-part> ::= \"Sr.\" | \"Jr.\" | <roman-numeral> | \"\""
    , "<opt-apt-num> ::= <apt-num> | \"\""
    ]

namePart :: String
namePart = Prelude.unlines
    [ "<name-part> ::= <personal-part> <last-name> <opt-suffix-part> <EOL>"
    , "            | <personal-part> <name-part>"
    ]
