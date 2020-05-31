module Main where

import Prelude

import Data.Either (Either(..))
import Data.Posix.Signal
import Effect (Effect)
import Effect.Console (log)
import Node.ReadLine (Interface, close, createConsoleInterface, noCompletion, question)
import Node.Process (stdin, stdout, onSignal)

main :: Effect Unit
main = do
  interface <- createConsoleInterface noCompletion
  question "What do you think of PureScript? " (callback interface) interface
  where
    callback :: Interface -> String -> Effect Unit
    callback interface answer = do
      log $ "Thank you for your valuable feedback: " <> answer
      close interface


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
