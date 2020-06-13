module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, clear)
import Node.Stream (Readable, Writable, write)
import Effect.Promise (Promise)
import Node.Process (stdin, stdout)
import Data.Function.Uncurried (Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Node.Encoding(Encoding(..))
import Node.Buffer(fromString, create)

foreign import consoleClear :: Effect Unit

-- ref: https://github.com/purescript-node/purescript-node-streams/blob/v4.0.1/src/Node/Stream.purs
foreign import onKeypressImpl :: Fn4 (Readable ()) (Writable ()) (Boolean) (PressedKeyInfo -> Effect Unit) (Effect Unit)
onKeypress :: Readable () -> Writable () -> Boolean -> (PressedKeyInfo -> Effect Unit) -> Effect Unit
onKeypress = runFn4 onKeypressImpl

main :: Effect Unit
main = do
  log "🍝"
  log "🍝"
  -- consoleClear
  log "🍝"
  log "🍝"
  onKeypress stdin stdout true $ \x -> do
                                   clear
                                   log $ show x


-- process.stdout.write('\033c') // clear console


type PressedKey = String

newtype PressedKeyInfo = PressedKeyInfo {
  sequence :: String
 ,name :: String
 ,ctrl :: Boolean
 ,meta :: Boolean
 ,shift :: Boolean
}
-- data PressedKeyInfo = PressedKeyInfo String String Boolean Boolean Boolean

derive instance genericPressedKeyInfo :: Generic PressedKeyInfo _
instance showPressedKeyInfo :: Show PressedKeyInfo where
  show (PressedKeyInfo {
  sequence : s
 ,name : n
 ,ctrl : c
 ,meta : m
 ,shift : sh
}) = "{ sequence: " <> s <> " ,name: " <> n <> " ,ctrl: " <> (show c) <> " ,meta: " <> (show m) <> " ,shift: " <> (show sh) <> " }"


-- foreign import onDataEitherImpl :: forall r . (Chunk -> Either String Buffer) -> Readable r -> (Either String Buffer -> Effect Unit) -> Effect Unit





