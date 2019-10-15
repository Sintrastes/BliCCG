{-# LANGUAGE DeriveLift #-}

module Lex (Prim(..),
            Slash(..),
            Cmplx(..),
            Lexicon,
            createLexicon,
            toFunc,
            parseCmplx) where

import qualified Data.Map as Map
import Data.List
import Control.Monad.Combinators (eitherP)
import Text.ParserCombinators.Parsec
import Language.Haskell.TH.Syntax

type Prim  = String
data Slash = Forw | Back deriving (Eq, Ord, Lift)

instance Show Slash where
    show Forw = "/"
    show Back = "\\"

data Cmplx = CmplxLeaf Prim |
             CmplxTree Cmplx Slash Cmplx deriving (Eq, Ord, Lift)

instance Show Cmplx where
    show (CmplxLeaf a)     = show a
    show (CmplxTree a b c) = "(" ++ show a ++ show b ++ show c ++ ")"

cmplxLeafP :: Parser Cmplx
cmplxLeafP = do
  try $ many space
  cs <- many $ alphaNum
  try $ many space
  return $ CmplxLeaf cs

cmplxP :: Parser Cmplx
cmplxP = try ( 
  do try $ many space
     x <- try cmplxParensP <|> cmplxLeafP
     try $ many space
     res <- (char '/') `eitherP` (char '\\')
     try $ many space
     y <- try cmplxParensP <|> cmplxLeafP
     try $ many space
     case res of
       Left  _ -> return $ CmplxTree x Forw y
       Right _ -> return $ CmplxTree x Back y)
 <|> try cmplxLeafP

parseCmplx :: String -> Either ParseError Cmplx
parseCmplx str = parse cmplxP "" str

cmplxParensP :: Parser Cmplx
cmplxParensP = do _ <- char '('
                  try $ many space
                  x <- try cmplxP
                  try $ many space
                  _ <- char ')'
                  return x 

newtype Func = Func [Prim]

instance Show Func where
  show (Func xs) = intercalate " -> " xs ++ "." 

-- Helper function to extract what a Cmplx looks like
-- as a function. Extracts a list of arguments in the order
-- they are expected.
toFunc :: Cmplx -> Func
toFunc (CmplxLeaf x) = Func [x]
toFunc (CmplxTree a _ b) = 
  let (Func b', Func a') = (toFunc b, toFunc a) 
  in Func $ b' ++ a'

type Lexicon = Map.Map String [Cmplx]

createLexicon :: [(String, [Cmplx])] -> Lexicon
createLexicon = Map.fromList