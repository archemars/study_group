module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Data.Foldable (fold)
import Data.Array (reverse)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Console (logShow)

import Node.Process (argv) as P
import Node.Yargs (argv)
import Node.Yargs.Setup (YargsSetup, usage, example)
import Node.Yargs.Applicative (flag, yarg, runY)


app :: Array String -> Boolean -> Effect Unit
app [] _     = pure unit
app ss false = logShow ss
app ss true  = logShow (reverse ss)



-- Error: Cannot find module 'yargs'
-- -> npm i yargs で治ったけど... npm i する必要あるん？
-- spago bundle-app -t output.js
-- node output.js -w hoge -w fuga -r

main :: Effect Unit
main = do
  args <- P.argv
  log $ show args

  let setup = usage "$0 -w Word1 -w Word2"
            <> example "$0 -w Hello -w World" "Say hello!"

  runY setup $ app <$> yarg "w" ["word"] (Just "A word") (Right "At least one word is required") false
         <*> flag "r" []       (Just "Reverse the words")

