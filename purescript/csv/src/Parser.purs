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

type ParseParam = { csv :: Array (Array String) , aq :: Boolean,  iqc :: Boolean, rteq :: Boolean }
initValue2 :: ParseParam
initValue2 = { csv: []
             , aq: false
             , iqc: false
             , rteq: false 
             }

parse :: String -> Delimiter -> Effect (Array (Array String))
parse s d = do
  let csv = foldl (\val -> \char -> readChar2 val char) initValue2 (split (Pattern "") (convertToCrlf s))
  pure csv.csv

addChar :: Array (Array String) -> Array String -> String -> String -> Array (Array String)
addChar csv row str char = csv <> [row <> [str <> char]]

addCell :: Array (Array String) -> Array String -> String -> Array (Array String)
addCell csv row cell = csv <> [row <> [cell]]

addRow :: Array (Array String) -> Array String -> Array (Array String)
addRow csv row = csv <> [row]

readChar2 :: ParseParam -> String -> ParseParam
readChar2 csv char = if csv.iqc == false then
                       baz2 csv char
                     else if char == "\"" && csv.aq == true && csv.iqc == true then
                       hoge2 csv char
                     else if csv.aq == true && csv.iqc == true && csv.rteq == true then
                       fuga2 csv char
                     else if csv.aq == true && csv.iqc == true && csv.rteq == false then
                       piyo2 csv char
                     else if char == "\"" && csv.aq == false && csv.iqc == true then
                       foo2 csv char
                     else if csv.aq == false && csv.iqc == true then
                       foofoo2 csv char
                     else
                       initValue2



hoge2 :: ParseParam -> String -> ParseParam
hoge2 pp char = { csv: addChar initCsv row str char
                , aq: false
                , iqc: pp.iqc
                , rteq: false
                }
      where
          initCsv = case init pp.csv of
               Just x -> x
               Nothing -> []
          lastRow = case last pp.csv of
                  Just x -> x
                  Nothing -> []
          row = case init lastRow of
                  Just x -> x
                  Nothing -> []
          str = case last lastRow of
                  Just x -> x
                  Nothing -> ""

fuga2 :: ParseParam -> String -> ParseParam
fuga2 pp char = { csv: resCsv
                , aq: false
                , iqc: if char /= "\"" then false else pp.iqc
                , rteq: false
                }
      where
          initCsv = case init pp.csv of
               Just x -> x
               Nothing -> []
          lastRow = case last pp.csv of
                  Just x -> x
                  Nothing -> []
          row = case init lastRow of
                  Just x -> x
                  Nothing -> []
          str = case last lastRow of
                  Just x -> x
                  Nothing -> ""
          resCsv = if char == "\"" then
                        addChar initCsv lastRow str char
                      else
                        if char == "," then -- "," is delimiter char
                            addCell initCsv (lastRow <> []) ""
                        else if char == "\n" then
                            addRow pp.csv []
                        else
                            pp.csv

piyo2 :: ParseParam -> String -> ParseParam
piyo2 pp char = { csv: addChar initCsv row str char
                , aq: false
                , iqc: pp.iqc
                , rteq: false
                }
      where
          initCsv = case init pp.csv of
               Just x -> x
               Nothing -> []
          lastRow = case last pp.csv of
                  Just x -> x
                  Nothing -> []
          row = case init lastRow of
                  Just x -> x
                  Nothing -> []
          str = case last lastRow of
                  Just x -> x
                  Nothing -> ""

foo2 :: ParseParam -> String -> ParseParam
foo2 pp char = { csv: pp.csv
                , aq: true
                , iqc: pp.iqc
                , rteq: true
                }

foofoo2 :: ParseParam -> String -> ParseParam
foofoo2 pp char = { csv: addChar initCsv row str char
                , aq: pp.aq
                , iqc: pp.iqc
                , rteq: pp.rteq
                }
  where
      initCsv = case init pp.csv of
           Just x -> x
           Nothing -> []
      lastRow = case last pp.csv of
              Just x -> x
              Nothing -> []
      row = case init lastRow of
              Just x -> x
              Nothing -> []
      str = case last lastRow of
              Just x -> x
              Nothing -> ""

baz2 :: ParseParam -> String -> ParseParam
baz2 pp char = { csv: csv
               , aq: pp.aq
               , iqc: iqc
               , rteq: pp.rteq
               }
    where
          initCsv = case init pp.csv of
               Just x -> x
               Nothing -> []
          lastRow = case last pp.csv of
                  Just x -> x
                  Nothing -> []
          row = case init lastRow of
                  Just x -> x
                  Nothing -> []
          str = case last lastRow of
                  Just x -> x
                  Nothing -> ""
          csv = if char == "," then
                  addCell initCsv (row <> []) str
                else if char == "\n" then
                  addRow (addCell initCsv row str) row
                else if char == "\"" then
                  pp.csv
                else
                  addChar initCsv row "" (str <> char)
          iqc = if char == "\"" then
                  true
                else 
                  pp.iqc


