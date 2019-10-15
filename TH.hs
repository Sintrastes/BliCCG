{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module TH where

import Lex
import Language.Haskell.TH
import Language.Haskell.TH.Quote

-- Quasiquoter for grammatical categories
cat :: QuasiQuoter
cat = QuasiQuoter {
    quoteExp  = parserTH
  , quotePat  = notHandled "patterns"
  , quoteType = notHandled "types"
  , quoteDec  = notHandled "declarations"
  }
  where notHandled things = error $
            things ++ " are not handled by the regex quasiquoter."
        parserTH :: String -> Q Exp
        parserTH s =
          case parseCmplx s of
            Left  err    -> fail (show err)
            Right x      -> [e| x |]
