module Parser where

import Prelude

import Effect (Effect)
import Data.Array (init, last)
import Data.Maybe (Maybe(Just, Nothing))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (regex, replace)
import Data.String.Regex.Flags (RegexFlags(..))
import Data.Foldable (foldl)
import Data.Either (Either(..))
import Effect.Ref (Ref, new, modify_, read)

type Delimiter = String
type AfterQuote = Boolean
type InsideQuoteCell = Boolean
type ReadyToEndQuote = Boolean

regexFlag :: RegexFlags
regexFlag = RegexFlags
  { global: true
  , ignoreCase: false
  , multiline: true
  , sticky: false
  , unicode: true
  }

convertToCrlf :: String -> String
convertToCrlf s = case regex "\r\n|\r|\n" regexFlag of
                    Left r -> "error"
                    Right r -> replace r "\n" s

initValue :: Array (Array String)
initValue = []

type ParseParam = { csv :: Array (Array String) , aq :: Boolean,  iqc :: Boolean, rteq :: Boolean }
initValue2 :: ParseParam
initValue2 = { csv: []
             , aq: false
             , iqc: false
             , rteq: false 
             }

parse :: String -> Delimiter -> Effect (Array (Array String))
parse s d = do
  raq <- new false
  riqc <- new false
  rrteq <- new false

  -- foldl (\val -> \char -> readChar val char raq riqc rrteq) (pure initValue) (split (Pattern "") (convertToCrlf s))
  let csv = foldl (\val -> \char -> readChar2 val char) initValue2 (split (Pattern "") (convertToCrlf s))
  pure csv

addChar :: Array (Array String) -> Array String -> String -> String -> Array (Array String)
addChar csv row str char = csv <> [row <> [str <> char]]

addCell :: Array (Array String) -> Array String -> String -> Array (Array String)
addCell csv row cell = csv <> [row <> [cell]]

addRow :: Array (Array String) -> Array String -> Array (Array String)
addRow csv row = csv <> [row]

readChar2 :: ParseParam -> String -> ParseParam
readChar2 csv char = if iqc == false then
                                   baz2 csv char raq riqc rrteq
                                 else if char == "\"" && aq == true && iqc == true then
                                   hoge2 csv char raq riqc rrteq
                                 else if aq == true && iqc == true && rteq == true then
                                   fuga2 csv char raq riqc rrteq
                                 else if aq == true && iqc == true && rteq == false then
                                   piyo2 csv char raq riqc rrteq
                                 else if char == "\"" && aq == false && iqc == true then
                                   foo2 csv char raq riqc rrteq
                                 else if aq == false && iqc == true then
                                   foofoo2 csv char raq riqc rrteq
                                 else
                                   []

readChar :: Effect (Array (Array String)) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
readChar _csv char raq riqc rrteq = do
  csv   <- _csv
  aq   <- read raq
  iqc  <- read riqc
  rteq <- read rrteq

  csv_ <- if iqc == false then
            baz csv char raq riqc rrteq
          else if char == "\"" && aq == true && iqc == true then
            hoge csv char raq riqc rrteq
          else if aq == true && iqc == true && rteq == true then
            fuga csv char raq riqc rrteq
          else if aq == true && iqc == true && rteq == false then
            piyo csv char raq riqc rrteq
          else if char == "\"" && aq == false && iqc == true then
            foo csv char raq riqc rrteq
          else if aq == false && iqc == true then
            foofoo csv char raq riqc rrteq
          else
            pure []

  pure csv_

  -- XXX TODO ↓のパターンを↑のifに入れる

hoge :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
hoge csv char raq riqc rrteq = do
  modify_ (\s -> false) raq
  modify_ (\s -> false) rrteq

  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

fuga :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
fuga csv char raq riqc rrteq = do
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let row = case last csv of
              Just x -> x
              Nothing -> []
  let str = case last row of
              Just x -> x
              Nothing -> ""
  aq   <- read raq

  modify_ (\s -> false) rrteq
  modify_ (\s -> false) raq

  iqc  <- read riqc
  if char /= "\"" then
    modify_ (\s -> false) riqc
  else
    modify_ (\s -> iqc) riqc

  let _csv_ = if char == "\"" then
                addChar csv_ row str char
              else
                if char == "," then -- "," is delimiter char
                    addCell csv_ (row <> []) ""
                else if char == "\n" then
                    addRow csv []
                else
                    csv
  pure _csv_

piyo :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
piyo csv char raq riqc rrteq = do
  modify_ (\s -> false) raq
  modify_ (\s -> false) rrteq

  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

foo :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
foo csv char raq riqc rrteq = do
  modify_ (\s -> true) raq
  modify_ (\s -> true) rrteq
  pure csv

foofoo csv char raq riqc rrteq = do
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

baz :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
baz csv char raq riqc rrteq = do
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  let _csv_ =
              if char == "," then
                addCell csv_ (row <> []) str
              else if char == "\n" then
                addRow (addCell csv_ row str) row
              else if char == "\"" then
                csv
              else
                addChar csv_ row "" (str <> char)

  iqc <- read riqc
  if char == "\"" then
    modify_ (\s -> true) riqc
  else 
    modify_ (\s -> iqc) riqc

  pure _csv_















hoge2 :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
hoge2 csv char raq riqc rrteq = do
  modify_ (\s -> false) raq
  modify_ (\s -> false) rrteq

  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

fuga2 :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
fuga2 csv char raq riqc rrteq = do
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let row = case last csv of
              Just x -> x
              Nothing -> []
  let str = case last row of
              Just x -> x
              Nothing -> ""
  aq   <- read raq

  modify_ (\s -> false) rrteq
  modify_ (\s -> false) raq

  iqc  <- read riqc
  if char /= "\"" then
    modify_ (\s -> false) riqc
  else
    modify_ (\s -> iqc) riqc

  let _csv_ = if char == "\"" then
                addChar csv_ row str char
              else
                if char == "," then -- "," is delimiter char
                    addCell csv_ (row <> []) ""
                else if char == "\n" then
                    addRow csv []
                else
                    csv
  pure _csv_

piyo2 :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
piyo2 csv char raq riqc rrteq = do
  modify_ (\s -> false) raq
  modify_ (\s -> false) rrteq

  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

foo2 :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
foo2 csv char raq riqc rrteq = do
  modify_ (\s -> true) raq
  modify_ (\s -> true) rrteq
  pure csv

foofoo2 csv char raq riqc rrteq = do
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

baz2 :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
baz2 csv char raq riqc rrteq = do
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  let _csv_ =
              if char == "," then
                addCell csv_ (row <> []) str
              else if char == "\n" then
                addRow (addCell csv_ row str) row
              else if char == "\"" then
                csv
              else
                addChar csv_ row "" (str <> char)

  iqc <- read riqc
  if char == "\"" then
    modify_ (\s -> true) riqc
  else 
    modify_ (\s -> iqc) riqc

  pure _csv_

