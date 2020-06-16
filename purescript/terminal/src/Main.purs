module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, clear)
import Node.Stream (Readable, Writable)
import Node.Process (stdin, stdout, argv)
import Data.Function.Uncurried (Fn1, runFn1, Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Array (replicate, length, head, reverse)
import Data.Traversable (for)
import Node.FS.Sync(readTextFile)
import Data.Maybe (Maybe(Just, Nothing))
import Node.Encoding(Encoding(UTF8))
import Data.Either (Either(..), either)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))


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
  let col = getColumns
  let row = getRows

  -- row文ファイルを読み込む
  -- 表示するファイル行数は `rowrange` で制御する
  -- どこかにファイル全体を持ってないと駄目かも
  a <- for (replicate row "a") \x -> log x

  onResize showSize
  onKeypress stdin stdout true getKeyName

  args <- argv
  log $ show args

  let fileName = case length args of
                   3 -> getFileName args
                   _ -> Nothing

  txt <- readTextFile UTF8 "hoge.txt"
  let p = Pattern "\n"
  let startRow = 0
  log $ displayTxt startRow row $ split p txt
  -- [{row: 1, txt: "あいうえお"}, {row: 2, txt: "かきくけこ"}] 的な配列にする必要があるかも
  -- filter (\x -> startRow < x.row && x.row < (startRow + row)) txts 的な
  -- 最後にjoinしましょ


getFileName :: Array String -> Maybe String
getFileName args = head $ reverse args

displayTxt :: Int -> Int -> Array String
displayTxt startRow row strs = for (replicate row "a") \x -> log x


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


getKeyName :: PressedKeyInfo -> Effect Unit
getKeyName (PressedKeyInfo pki) = do
  clear
  let name = case pki.name of
       "h" -> "left"
       "j" -> "down"
       "k" -> "up"
       "l" -> "right"
       _   -> pki.name
  log name

