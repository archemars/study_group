module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, clear, logShow)
import Node.Stream (Readable, Writable)
import Node.Process (stdin, stdout, argv)
import Data.Function.Uncurried (Fn1, runFn1, Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Array (replicate, length, head, reverse, filter, snoc, zip)
import Data.String (length) as S
import Data.Traversable (for)
import Node.FS.Sync(readTextFile)
import Data.Maybe (Maybe(Just, Nothing))
import Node.Encoding(Encoding(UTF8))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Foldable (foldl)
import Data.Tuple (fst, snd)
import Effect.Ref as R

import Parser (parse)


foreign import getColumns :: Int
foreign import getRows :: Int

-- ref: https://github.com/purescript-node/purescript-node-streams/blob/v4.0.1/src/Node/Stream.purs
foreign import onKeypressImpl :: Fn4 (Readable ()) (Writable ()) (Boolean) (PressedKeyInfo -> Effect Unit) (Effect Unit)
onKeypress :: Readable () -> Writable () -> Boolean -> (PressedKeyInfo -> Effect Unit) -> Effect Unit
onKeypress = runFn4 onKeypressImpl

foreign import onResizeImpl :: Fn1 (Int -> Int -> Effect Unit) (Effect Unit)
onResize :: (Int -> Int -> Effect Unit) -> Effect Unit
onResize = runFn1 onResizeImpl

main :: Effect Unit
main = do
  let screenCol = getColumns
  let screenRow = getRows/2

  onResize showSize
  args <- argv

  let fileName = case length args of
                   3 -> getFileName args
                   _ -> Nothing


  let fn = case fileName of
             Just hoge -> hoge
             _ -> ""
  txt <- readTextFile UTF8 fn
  let txtRecord = createTextRecord txt
  let startRow = 0
  srs <- R.new 0
  onKeypress stdin stdout true (displayTxt screenRow txtRecord srs)

  csv <- parse txt "asdf"
  -- logShow initCsv
  -- logShow CSV
  --   { csv : getCsv csv
  --   , columnWidth : getCsvWidth csv
  --   }

  -- logShow csv
  logShow csv
  -- logShow $ getCsvWidth csv
  logShow $ getCsv csv -- TODO aaaaaaaaaaaaaaaa can use this ?

getCsv :: Array (Array String) -> Array Row
getCsv csv = foldl (\val -> \acc -> 
               val <> Row { row : getRow acc
                          , maxHeight: max (map (\x -> length (split (Pattern "\n") x)) acc)
                          }
               ) [] csv

getCsvWidth :: Array (Array String) -> Array Int
getCsvWidth csv = foldl (\val -> \acc -> 
                    if length acc == 0 then
                      val
                    else if length val == 0 then
                      map S.length acc
                    else map (\x -> if fst x < snd x then snd x else fst x) (zip val (map S.length acc))) [] csv

getRow :: Array String -> Array Cell
getRow csv = map (\v -> Cell { text: v
                             , padingText: "" <> v <> ""
                             , maxHeight: 0
                             }
                 ) csv

newtype CSV = CSV
  { csv:: Array Row
  , columnWidth:: Array Int
  }

newtype Row = Row
  { row :: Array Cell
  , maxHeight :: Int
  }

newtype Cell = Cell
  { text :: String
  , paddingText :: String
  , maxHeight :: Int
  }

initCsv :: CSV
initCsv = CSV
  { csv: []
  , columnWidth: []
  }


sepalater :: Pattern
sepalater = Pattern ","

showTxtRecord :: Array TxtRecord -> Effect Unit
showTxtRecord t = do
  let firstRow = case head t of
              Just h -> h.char
              Nothing -> ""

  -- top line
  log $ "+ " <> (foldl (\x -> \_ -> x <> "-") "" $ replicate (((S.length firstRow) * 2) - 2) "-") <> " +"
  a <- for t \x -> do
     let columns = split sepalater x.char
     let rowLength = S.length x.char + length columns
     log $ "+ " <> (foldl (\x -> \_ -> x <> "-") "" $ replicate ((S.length x.char * 2) - 2) "-") <> " +"
     log $ "| " <> (foldl (\x -> \y -> if x == "" then y else x <> " | " <> y) "" columns) <> " |"

  -- bottom line
  log $ "+ " <> (foldl (\x -> \_ -> x <> "-") "" $ replicate (((S.length firstRow) * 2) - 2) "-") <> " +"

type TxtRecord = {row:: Int, char:: String}
initTxtRecord :: Array TxtRecord
initTxtRecord = []

createTextRecord :: String -> Array TxtRecord
createTextRecord txt =  foldl (\x -> \y -> snoc x {row: (length x + 1), char: y}) initTxtRecord (split (Pattern "\n") txt)

getFileName :: Array String -> Maybe String
getFileName args = head $ reverse args

displayTxt :: Int -> Array TxtRecord -> (R.Ref Int) -> PressedKeyInfo -> Effect Unit
displayTxt screenRow txtReords srs (PressedKeyInfo pki) = do
  let startRow = 0
  clear
  let name = case pki.name of
       "h" -> "left"
       "j" -> "down"
       "k" -> "up"
       "l" -> "right"
       _   -> pki.name
  case name of
       "down" -> R.modify_ (\s -> if (s + screenRow) > (length txtReords) then s else s + 1) srs
       "up" -> R.modify_ (\s -> if s < 1 then s else s - 1) srs
       _   -> R.modify_ (\s -> s) srs
  newStartRow  <- R.read srs
  let displayTxt = filter (\x -> newStartRow < x.row && x.row < (newStartRow + screenRow)) txtReords
  showTxtRecord displayTxt


showSize :: Int -> Int -> Effect Unit
showSize col row = do
  log "col"
  log $ show col
  log "row"
  log $ show row

newtype PressedKeyInfo = PressedKeyInfo {
  sequence :: String
 ,name :: String
 ,ctrl :: Boolean
 ,meta :: Boolean
 ,shift :: Boolean
}

derive instance genericPressedKeyInfo :: Generic PressedKeyInfo _
instance showPressedKeyInfo :: Show PressedKeyInfo where
  show (PressedKeyInfo {
  sequence : s
 ,name : n
 ,ctrl : c
 ,meta : m
 ,shift : sh
}) = "{ sequence: " <> (show s) <> " ,name: " <> n <> " ,ctrl: " <> (show c) <> " ,meta: " <> (show m) <> " ,shift: " <> (show sh) <> " }"


getKeyName :: Int -> PressedKeyInfo -> Effect Unit
getKeyName i (PressedKeyInfo pki) = do
  clear
  let name = case pki.name of
       "h" -> "left"
       "j" -> "down"
       "k" -> "up"
       "l" -> "right"
       _   -> pki.name
  log name
  log $ show i


derive instance genericCSV :: Generic CSV _
instance showCSV :: Show CSV where
  show (CSV {
    csv: c
  , columnWidth: cl
  }) = "{ csv: " <> (show c) <> " ,columnWidth: " <> (show cl) <> " }"

derive instance genericRow :: Generic Row _
instance showRow :: Show Row where
  show (Row {
    row : r
  , maxHeight : mh
  }) = "{ row: " <> (show r) <> " ,maxHeight: " <> (show mh) <> " }"

derive instance genericCell :: Generic Cell _
instance showCell :: Show Cell where
  show (Cell {
    text : t
  , paddingText : pt
  }) = "{ test: " <> t <> " ,paddingText: " <> pt <> " }"
