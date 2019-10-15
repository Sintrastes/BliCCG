
import Parser
import Lex

-- Note: This gives two parses, the first of which is invalid.
lex1 = createLexicon [
   ("I", [CmplxLeaf "entity"]),
   ("am", [CmplxTree (CmplxTree (CmplxLeaf "prop") Forw (CmplxLeaf "entity")) Back (CmplxLeaf "entity")]),
   ("a",[CmplxTree (CmplxLeaf "entity") Forw (CmplxLeaf "entity")]),
   ("parser",[CmplxLeaf "entity"])
   ]

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