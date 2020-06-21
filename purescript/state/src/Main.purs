module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log, logShow)
import Control.Monad.State
import Data.List
import Data.Tuple
import Data.Maybe

main :: Effect Unit
main = do
  logShow $ runState manip (3 : 2 : 1 : Nil)


-- main :: Effect Unit
-- main = do
--   log $ runState popState 5

-- test = do
--   log "🍝"
--   a <- get 0
--   put $ a + 1
--   modify ((*) 2)
--   pure a

pushState :: Int -> State (List Int) Unit
pushState x = modify_ (\s -> x : s)

popState :: State (List Int) (Maybe Int)
popState = do
  xs <- get
  modify_ $ drop 1
  pure $ head xs

manip :: State (List Int) Unit
manip = do
    pushState 4
    _ <- popState
    _ <- popState
    pure unit

