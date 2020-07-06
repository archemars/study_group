module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, clear, logShow)
import Node.Stream (Readable, Writable)
import Node.Process (stdin, stdout, argv)
import Data.Function.Uncurried (Fn1, runFn1, Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Array (replicate, length, head, reverse, filter, snoc)
import Data.Traversable (for)
import Node.FS.Sync(readTextFile)
import Data.Maybe (Maybe(Just, Nothing))
import Node.Encoding(Encoding(UTF8))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Foldable (foldl)
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


