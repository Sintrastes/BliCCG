module Parser where

import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Lex
import Rules

-- | Datastructure used internally here.
data ParseTree = Leaf String | Cmplx :^ [ParseTree] 
             deriving (Eq, Ord)

instance Show ParseTree where
    show (Leaf a)     = a
    show (c :^ [x])   = show c ++ "^" ++ show x
    show (c :^ trees) = show c ++ "^" ++ "(" ++ intercalate " " (map show trees) ++ ")"

-- | Datastructure I need for my own processing.
data ApplicationTree = Id String | Branch [ApplicationTree]

-- | Get all the relevant data from the parse tree.
toApplicationTree :: ParseTree -> ApplicationTree
toApplicationTree (Leaf x) = Id x
toApplicationTree (c :^ xs) = Branch $ map toApplicationTree xs

instance Show ApplicationTree where
    show (Id a)     = a
    show (Branch [x])   = show x
    show (Branch trees) = "(" ++ intercalate " " (map show trees) ++ ")"


type Cell   = Map.Map Cmplx [ParseTree]
type Vector = [(Int, Cell)]

(??) :: Ord s => Map.Map s [a] -> s -> [a]
m ?? s = fromMaybe [] (Map.lookup s m)

parse :: Lexicon -> [String] -> [ParseTree]
parse lexicon = process
  where
    process :: [String] -> [ParseTree]
    process input
        | size == ncell = cell ?? CmplxLeaf "prop"
        | otherwise     = []
      where (size, vectors) = foldl nextInputToken (0, []) input
            (ncell, cell)   = last (last vectors)

    nextInputToken :: (Int, [Vector]) -> String -> (Int, [Vector])
    nextInputToken (size, vectors) token = (size', vectors')
      where size'    = size + 1
            vectors' = [(size', cell)] : updateVectors vectors [(size, cell)] size size'
            cell     = terminalCell token

    updateVectors :: [Vector] -> Vector -> Int -> Int -> [Vector]
    updateVectors [] _ _ _ = []
    updateVectors (row:rows) col nrow ncol 
        | scProduct == Map.empty =  row                    : updateVectors rows                  col  nrow' ncol
        | otherwise            = (row++[(ncol,scProduct)]) : updateVectors rows ((nrow',scProduct):col) nrow' ncol
      where scProduct = scalarProduct row col
            nrow'   = nrow - 1

    scalarProduct :: Vector -> Vector -> Cell
    scalarProduct [] _ = Map.empty
    scalarProduct _ [] = Map.empty
    scalarProduct as@((i,a):as') bs@((j,b):bs')
        = case compare i j of
            LT -> scalarProduct as' bs
            GT -> scalarProduct as  bs'
            EQ -> scalarProduct as' bs' `joinCell` binProduct a b

    joinCell :: Cell -> Cell -> Cell
    joinCell a b = Map.unionsWith (++) [a, b]

    terminalCell :: String -> Cell
    terminalCell term = Map.fromList [ (cat, treesFor cat) |
                 cat <- lexicon Map.! term ]
      where treesFor cat = [cat:^[Leaf term]]

    binProduct :: Cell -> Cell -> Cell
    binProduct acell bcell = Map.unionsWith (++) [ Map.fromList [(c, [c:^ (atree++btree) ])] |
                         (a, atree) <- Map.toList acell,
                         (b, btree) <- Map.toList bcell,
                         c          <- applyRules a b ]
