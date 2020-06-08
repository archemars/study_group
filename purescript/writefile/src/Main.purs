module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.FS.Async(writeTextFile)
import Node.Process (argv)
import Data.Array (reverse, head, length)
import Data.Maybe (Maybe(Just, Nothing))
import Node.Encoding(Encoding(UTF8))
import Data.Either (Either(..), either)


-- spago run -a hoge.txt
main :: Effect Unit
main = do
  args <- argv
  log $ show args

  let fileName = case length args of
                   3 -> getFileName args
                   _ -> Nothing

  case fileName of
    Just x -> writeTextFile UTF8 x "write this text\n" $ \_ -> do
                                                         log $ "\n\nWRITE FILE IN " <> x
    Nothing -> log "errrrrr"


getFileName :: Array String -> Maybe String
getFileName args = head $ reverse args

