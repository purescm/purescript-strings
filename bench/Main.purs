module Bench.Main where

import Prelude

import Data.String.CodePoints as CP
import Data.String.CodeUnits as CU
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Console (log)
import Performance.Minibench (benchWith)

main :: Effect Unit
main = do
  input <- readTextFile "./bench/alice.txt"
  inputUnicode <- readTextFile "./bench/alice.unicode.txt"

  let
    inputChez = toChezString input
    inputChezUnicode = toChezString inputUnicode

  log "Chez native string string-length ascii" 
  benchWith 100 \_ -> lengthNative inputChez
  log "---"

  log "String length (code units) ascii"
  benchWith 100 \_ -> CU.length input
  log "---"

  log "Chez native string string-length unicode" 
  benchWith 100 \_ -> lengthNative inputChezUnicode
  log "---"

  log "String length (code units) unicode"
  benchWith 100 \_ -> CU.length inputUnicode
  log "---"

  log "String length (code points) ascii"
  benchWith 100 \_ -> CP.length input
  log "---"

  log "String length (code points) unicode"
  benchWith 100 \_ -> CP.length inputUnicode
  log "---"

  log "Chez native string string-append (concat)"
  benchWith 100 \_ -> runFn2 stringAppend inputChezUnicode inputChezUnicode
  log "---"
  
  log "String concat"
  benchWith 100 \_ -> inputUnicode <> inputUnicode

foreign import data ChezString :: Type

foreign import toChezString :: String -> ChezString

foreign import readTextFile :: String -> Effect String

foreign import lengthNative :: ChezString -> Int

foreign import stringAppend :: Fn2 ChezString ChezString ChezString

