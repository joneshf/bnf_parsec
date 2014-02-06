{-# LANGUAGE DeriveDataTypeable, FlexibleContexts #-}
module BNF where

import Control.Applicative ((<*>), (<$>), (<*))
import Data.Generics
import Data.List.NonEmpty
import Text.Parsec

data BNF = BNF (NonEmpty Production)
    deriving (Eq, Show, Typeable, Data)

data Production = Production String (NonEmpty BNFExpr)
    deriving (Eq, Show, Typeable, Data)

data BNFExpr = BNFExpr (NonEmpty BNFBase)
    deriving (Eq, Show, Typeable, Data)

data BNFBase = NonTerminal String
             | Terminal String
    deriving (Eq, Show, Typeable, Data)

bnf :: Stream s m Char => ParsecT s u m BNF
bnf = BNF <$> parsecMap fromList (many1 production)

production :: Stream s m Char => ParsecT s u m Production
production = Production <$> anyBetween (char '<') (char '>') <* (spaces >> string "::=" >> spaces)
                        <*> parsecMap fromList (bnfExpr `sepBy1` (spaces >> char '|' >> spaces))

bnfExpr :: Stream s m Char => ParsecT s u m BNFExpr
bnfExpr = BNFExpr <$> parsecMap fromList ((nonTerminal <|> terminal) `sepEndBy1` spaces)

nonTerminal :: Stream s m Char => ParsecT s u m BNFBase
nonTerminal = NonTerminal <$> anyBetween (char '<') (char '>')

terminal :: Stream s m Char => ParsecT s u m BNFBase
terminal = Terminal <$> anyBetween (char '"') (char '"')

anyBetween :: Stream s m Char
           => ParsecT s u m open
           -> ParsecT s u m close
           -> ParsecT s u m String
anyBetween open close = open >> manyTill anyChar (try close)
