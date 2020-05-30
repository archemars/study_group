module Main where

import Prelude

import Data.Either (Either(..))
import Data.Options ((:=))
import Effect (Effect)
import Effect.Aff (Aff, Canceler, Error, makeAff, nonCanceler, runAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question, createInterface, output)
import Node.Process (stdin, stdout)


-- main :: Effect Unit
-- main = do
--   interface <- createConsoleInterface noCompletion 
--   runAff_ (close' interface) (question' interface)
--   where
--     close' :: forall a. Interface -> Either Error a -> Effect Unit
--     close' interface = (\_ -> close interface)
-- 
--     question' :: Interface -> Aff Unit
--     question' interface = do
--       answer <- questionAsync message1 interface
--       liftEffect <<< log $ message2 <> answer
--       where
--         message1 = "What do you think of PureScript????? "
--         message2 = "Thank you for your valuable feedback: "
-- 
-- questionAsync :: String -> Interface -> Aff String
-- questionAsync message interface = makeAff go
--   where
--   go :: (Either Error String -> Effect Unit) -> Effect Canceler
--   go callback = question message (callback <<< Right) interface $> nonCanceler

-- main :: Effect Unit
-- main = do
--   rll <- createConsoleInterface noCompletion
--   rl <- createInterface stdin $ output := stdout
--   question "????" (\s -> do
--     log s
--     close rl) rll

-- question :: String -> (String -> Effect Unit) -> Interface -> Effect Unit

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  question "What do you think of PureScript? " (callback interface) interface
  where
    callback :: Interface -> String -> Effect Unit
    callback interface answer = do
      log $ "Thank you for your valuable feedback: " <> answer
      close interface

