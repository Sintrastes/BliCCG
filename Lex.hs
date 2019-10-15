module Lex (Prim(..),
            Slash(..),
            Cmplx(..),
            Lexicon,
            createLexicon,
            toFunc) where

import qualified Data.Map as Map
import Data.List

type Prim  = String
data Slash = Forw | Back deriving (Eq, Ord)

instance Show Slash where
    show Forw = "/"
    show Back = "\\"

data Cmplx = CmplxLeaf Prim |
             CmplxTree Cmplx Slash Cmplx deriving (Eq, Ord)

instance Show Cmplx where
    show (CmplxLeaf a)     = show a
    show (CmplxTree a b c) = "(" ++ show a ++ show b ++ show c ++ ")"

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