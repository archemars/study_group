module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, clear,  logShow)
import Node.Stream (Readable, Writable)
import Node.Process (stdin, stdout, argv)
import Data.Function.Uncurried (Fn1, runFn1, Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Array (replicate, length, head, reverse, filter, snoc)
import Data.Traversable (for)
import Node.FS.Sync(readTextFile)
import Data.Maybe (Maybe(Just, Nothing))
import Node.Encoding(Encoding(UTF8))
import Data.Either (Either(..), either)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Foldable (foldl)

import Control.Monad.Writer (writer)
import Data.Tuple (Tuple)
import Control.Monad.State
import Control.Monad.State.Class
import Effect.Ref as R



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
  let screenRow = getRows

  -- row文ファイルを読み込む
  -- 表示するファイル行数は `rowrange` で制御する
  -- どこかにファイル全体を持ってないと駄目かも
  -- a <- for (replicate screenRow "a") \x -> log x

  onResize showSize
  -- onKeypress stdin stdout true (getKeyName 1)

  args <- argv
  -- log $ show args

  let fileName = case length args of
                   3 -> getFileName args
                   _ -> Nothing

  txt <- readTextFile UTF8 "hoge.txt"
  let txtRecord = createTextRecord txt
  let startRow = 0
  -- let displayTxtiiii = filter (\x -> startRow < x.row && x.row < (startRow + screenRow)) txtRecord
  -- showTxtRecord displayTxtiiii
  -- log "i"

  -- let srs = runState manip2 0
  -- let srs = startRowState
  srs <- R.new 0
  onKeypress stdin stdout true (displayTxt screenRow txtRecord srs)

  -- log $ show txtRecord
  -- [{row: 1, txt: "あいうえお"}, {row: 2, txt: "かきくけこ"}] 的な配列にする必要があるかも
  -- 最後にjoinしましょ
  -- log $ displayTxt startRow row $ split p txt

showTxtRecord :: Array TxtRecord -> Effect Unit
showTxtRecord t = do
  a <- for t \x -> do
     log x.char
  log ""

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
  let newStartRow = case name of
       "left"  -> screenRow
       "down" -> screenRow + 1
       "up" -> screenRow - 1
       "right" -> screenRow
       _   -> screenRow
  let displayTxt = filter (\x -> newStartRow < x.row && x.row < (newStartRow + screenRow)) txtReords
  -- showTxtRecord displayTxt
  R.modify_ (\s -> s + 1) srs
  i <- R.read srs
  logShow i
  -- modify_ (_ + 1)
  -- logShow $ runState srs 0
  -- logShow $ runState (manimani name) 0


  -- plusState 1
  -- _ <- minusState

  -- logShow $ runState manip2 0

-- main :: Effect Unit
-- main = do
--   -- logShow $ runState manip (3 : 2 : 1 : Nil)
--   logShow $ runState manip2 0

startRowState :: Tuple Unit Int
startRowState = runState (pure unit) 0

plusState :: Int -> State Int Unit
plusState x = modify_ (\s -> s + x)

minusState :: State Int Int
minusState = do
  xs <- get
  modify_ (\x -> xs - 1)
  pure xs

pp :: State Int Int
pp = do
    xs <- get
    plusState 1
    pure xs

ppp :: State Int Unit
ppp = do
    xs <- get
    plusState 1
    pure unit

manip2 :: State Int Unit
manip2 = do
    plusState 1
    plusState 10
    plusState 4
    _ <- minusState
    pure unit

manimani :: String -> State Int Unit
manimani s = do
    xs <- get
    _ <- minusState
    if s == "down" then
      plusState 1
    else
      plusState 10
    pure unit



-- modStartRow :: Int -> State Int Unit
-- modStartRow i = modify (\sum -> sum)
-- TODO XXX startRowのいちを保持しておきたいけどやりかたがわからん
-- stateモナドかreader writer モナドを使えばいい感じにできそうなんだけども

type RowRange = {
   start:: Int
  ,end:: Int
}

rowRange :: RowRange
rowRange = {start: 0, end: 0}

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


