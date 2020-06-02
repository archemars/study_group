module Main where

import Prelude

-- import Data.Either (Either(..))
import Data.Options ((:=))
-- import Data.Show (Buffer)
-- import Data.Posix.Signal
import Effect (Effect)
-- import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.ReadLine (Interface, close, completer, prompt, createConsoleInterface, noCompletion, question, createInterface, output, setPrompt, terminal)
import Node.Process (stdin, stdout, onSignal)
import Node.Stream (onData, onDataString, end, cork)
import Node.Buffer (Buffer, toString)
import Node.Encoding  (Encoding(..))
-- import Web.UIEvent.KeyboardEvent as KE
-- import Web.Event.Event (EventType(..))
-- import Web.Event.Internal.Types (Event)
-- import Web.Event.EventTarget (addEventListener, eventListener)


main :: Effect Unit
main = do

  -- keypressっぽいタイミングで入力キーをhexで出力
  intfc <- createInterface stdin $ terminal := true
  onData stdin (\s -> do
          str <- toString Hex s
          log $ show $ str == "1b5b42"
          log str
          )
  log "BBB"









  -- READ STREAM
  -- onData stdin (\s -> do
  --         str <- toString Hex s
  --         log str
  --         )

  -- onDataString stdin UTF8 (\s -> do
  --         log s
  --         end stdout (log "AAA")
  --         )

  -- READ LINE
  -- rl <- createInterface stdin $ output := stdout
  -- question "????" (\s -> do
  --   log s
  --   close rl) rl
  -- read line prompt
  -- prompt $ createConsoleInterface completer
  -- setPrompt "hoge" 3 intfc
  -- setPrompt "fuga" 2 intfc
  -- prmpt <-  prompt intfc
-- stdin
-- onData
-- onDataString
{-
var stdin = process.openStdin();
require('tty').setRawMode(true);

stdin.on('keypress', function (chunk, key) {
  process.stdout.write('Get Chunk: ' + chunk + '\n');
  if (key && key.ctrl && key.name == 'c') process.exit();
});
-}



-- main :: Effect Unit
-- main = do
--   interface <- createConsoleInterface noCompletion
--   question "What do you think of PureScript? " (callback interface) interface
--   where
--     callback :: Interface -> String -> Effect Unit
--     callback interface answer = do
--       log $ "Thank you for your valuable feedback: " <> answer
--       close interface
-- 
-- main :: forall e. Effect (console :: CONSOLE, readline :: READLINE, exception :: EXCEPTION | e) Unit
-- main :: Effect Unit
-- main = do
--   interface <- createConsoleInterface noCompletion
--   runAff_ (either (error <<< show) log) (runReaderT loop interface)
--   where
--     loop = do
--       setPrompt "$ "
--       dog <- question "What's your dog's name?\n"
--       liftEffect <<< log $ "Can I pet " <> dog <> "?"
--       str <- readLine
--       case uncons str of
--         Just {head: 'y'} -> liftEffect $ log "Thanks!"
--         _ -> liftEffect $ log "C'mon! Be a sport about it!"
--       loop

-- test a =
--   log "Key Pressed : "
--   -- case runExcept (KE.eventToKeyboardEvent a) of
--   --   Left err ->
--   --     log "Event was not a keyboard event"
--   --   Right ke -> do
--   --     let co = KE.key ke
--   --     log "Key Pressed : "
-- 
-- main = do
--   -- documenttarget <- liftEffect $ window >>= document <#> DHT.htmlDocumentToEventTarget
--   addEventListener (EventType "keydown") (eventListener test) true (documenttarget)


