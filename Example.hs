{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Example where

import Parser
import Lex
import TH
import Data.List

myList = [("I",      [[cat| entity ]])
   ,("a",      [[cat| entity/entity |]])
   ,("am",     [[cat| (prop/entity)\entity |]])
   ,("parser", [[cat| entity |]])]

myList2 = [("a",      [[cat| entity/entity |]])]

-- Note: This gives two parses, the first of which is invalid.
lexEx = createLexicon (myList)

result = Parser.parse lexEx ["I", "am", "a", "parser"]

-- Note: Although, something like this might
-- actually be good. For instance, we can parse:
-- "I am a" -> \x -> am(i, a(x))

-- This example shows some lexical ambiguity.
-- Note: I should probaby have a better syntax for this first, though.
{-
lex2 = createLexicon [
  ("I", [ComplxLeaf "entity"]),
  ("killed", [ComplxTree (ComplxTree "pred[entity]" "pred[entity,instr]") Back (CmplxLeaf "entity") ]),
  ("the", [CmplxTree (CmplxLeaf "entity") Forw (CmplxLeaf "entity")]),
  ("man", [ComplxLeaf "entity"]),
  ("with", [
           , 
           ]),
  ("a", [CmplxTree (CmplxLeaf "entity") Forw (CmplxLeaf "entity")]),
  ("spoon", [CmplxLeaf "entity"])
  ]
-}