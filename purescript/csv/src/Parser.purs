module Parser where

import Prelude

import Effect (Effect)
import Effect.Console (log, clear, logShow, error)
import Node.Stream (Readable, Writable)
import Node.Process (stdin, stdout, argv)
import Data.Function.Uncurried (Fn1, runFn1, Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Array (replicate, length, head, reverse, filter, snoc, cons, init, tail, last)
import Data.String (length) as S
import Data.Traversable (for)
import Node.FS.Sync(readTextFile)
import Data.Maybe (Maybe(Just, Nothing))
import Node.Encoding(Encoding(UTF8))
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (Regex, regex, replace)
import Data.String.Regex.Flags (RegexFlags(..), RegexFlagsRec, noFlags)
import Data.Foldable (foldl)
import Data.Either (Either(..), fromRight)
import Effect.Ref (new, modify_, Ref, read)

hogehoge :: String
hogehoge = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

-- public enum Delimiter
-- {
-- 	Comma,
-- 	Tab
-- }
type Delimiter = String
type AfterQuote = Boolean
type InsideQuoteCell = Boolean
type ReadyToEndQuote = Boolean


-- static void ConvertToCrlf(ref string data)
-- {
-- 	data = Regex.Replace(data, @"\r\n|\r|\n", "\r\n");
-- }
regexFlag :: RegexFlags
regexFlag = RegexFlags
  { global: true
  , ignoreCase: false
  , multiline: true
  , sticky: false
  , unicode: true
  }


convertToCrlf :: String -> String
convertToCrlf s = case regex "\r\n|\r|\n" regexFlag of
                    Left r -> "errrrrr"
                    Right r -> replace r "\n" s

initValue :: Array (Array String)
initValue = []

-- TODO column validate?

-- static readonly Dictionary<Delimiter, char> Delimiters = new Dictionary<Delimiter, char>() {{Delimiter.Comma, ','}, {Delimiter.Tab, '\t'}};
parse :: String -> Delimiter -> Effect (Array (Array String))
parse s d = do
  raq <- new false
  riqc <- new false
  rrteq <- new false

  foldl (\val -> \char -> readChar val char raq riqc rrteq) (pure initValue) (split (Pattern "") (convertToCrlf s))

addChar :: Array (Array String) -> Array String -> String -> String -> Array (Array String)
addChar csv row str char = csv <> [row <> [str <> char]]

addCell :: Array (Array String) -> Array String -> String -> Array (Array String)
addCell csv row cell = csv <> [row <> [cell]]

addRow :: Array (Array String) -> Array String -> Array (Array String)
addRow csv row = csv <> [row]

-- createTextRecord txt =  foldl (\x -> \y -> snoc x {row: (length x + 1), char: y}) initTxtRecord (split (Pattern "\n") txt)

-- 次はafterquote とか insidequoteとかごとのcaseを追加？
readChar :: Effect (Array (Array String)) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
readChar _csv char raq riqc rrteq = do
  csv   <- _csv
  aq   <- read raq
  iqc  <- read riqc
  rteq <- read rrteq

  log "-----"
  log $ "aq  : " <> (show aq  )
  log $ "iqc : " <> (show iqc )
  log $ "rteq: " <> (show rteq)

  log $ "char: " <> (show char)
  log $ "before: " <> (show csv)
  if char == "\"" && aq == true && iqc == true then
    log "route hoge"
  else if aq == true && iqc == true && rteq == true then
    log "route fuga"
  else if aq == true && iqc == true && rteq == false then
    log "route piyo"
  else if char == "\"" && aq == false then
    log "route foo"
  else if aq == false then
    log "route bar"
  else if iqc == false then
    log "route baz"
  else
    log "route eles"
  -- csv_ <- caseParse csv char aq iqc iteq
  csv_ <- if iqc == false then
            baz csv char raq riqc rrteq
          else if char == "\"" && aq == true && iqc == true then
            hoge csv char raq riqc rrteq
          else if aq == true && iqc == true && rteq == true then
            fuga csv char raq riqc rrteq
          else if aq == true && iqc == true && rteq == false then
            piyo csv char raq riqc rrteq
          else if char == "\"" && aq == false && iqc == true then
            foo csv char raq riqc rrteq
          else if aq == false && iqc == true then
            foofoo csv char raq riqc rrteq
          -- else if aq == false then
          --   bar csv char raq riqc rrteq
          else
            pure []

  log $ "after: " <> (show csv_)
  pure csv_

  -- XXX TODO ↓のパターンを↑のifに入れる


-- caseParse :: Effect Array (Array String) -> String -> AfterQuote -> InsideQuoteCell -> ReadyToEndQuote -> Effect Array (Array String)
-- caseParse csv "\"" (Ref true) true _ = hoge csv "\""
-- caseParse csv char true true true = fuga csv char
-- caseParse csv char true true false = piyo csv char
-- caseParse csv "\"" false _ _ = foo csv "\""
-- caseParse csv char false _ _ = bar csv char
-- caseParse csv char _ false _ = baz csv char
-- caseParse csv char _ _ _ = pure csv

hoge csv char raq riqc rrteq = do
  modify_ (\s -> false) raq -- TODO ref bool afterQuote
  modify_ (\s -> false) rrteq -- TODO ref bool readyToEndQuote

  -- let csv_ = init csv
  -- let row = tail csv
  -- let str = tail row
  -- let csv_ = case init csv of
  --              Just x -> x
  --              Nothing -> []
  -- let row = case last csv of
  --             Just x -> x
  --             Nothing -> []
  -- let str = case last row of
  --             Just x -> x
  --             Nothing -> ""
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

fuga csv char raq riqc rrteq = do
  -- let csv_ = init csv
  -- let row = tail csv
  -- let str = tail row
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let row = case last csv of
              Just x -> x
              Nothing -> []
  let str = case last row of
              Just x -> x
              Nothing -> ""
  -- let csv_ = case init csv of
  --              Just x -> x
  --              Nothing -> []
  -- let lastRow = case last csv of
  --             Just x -> x
  --             Nothing -> []
  -- let row = case init lastRow of
  --             Just x -> x
  --             Nothing -> []
  -- let str = case last lastRow of
  --             Just x -> x
  --             Nothing -> ""
  aq   <- read raq

  modify_ (\s -> false) rrteq -- TODO ref bool readyToEndQuote
  modify_ (\s -> false) raq -- TODO ref bool afterQuote

  iqc  <- read riqc
  if char /= "\"" then
    modify_ (\s -> false) riqc -- TODO ref bool insideQuoteCell
  else
    modify_ (\s -> iqc) riqc -- TODO ref bool insideQuoteCell

  let _csv_ = if char == "\"" then
                -- let csv_ = init csv
                -- let row = tail csv
                -- let str = tail row
                addChar csv_ row str char
                -- csv
              else
                -- if char == delimiterChar then TODO
                if char == "," then
                    -- addCell row cell

                    -- let csv_ = init csv
                    -- let row = tail csv
                    -- let str = tail row
                    addCell csv_ (row <> []) ""
                else if char == "\n" then
                    -- addRow csv_ (row <> []) ""
                    -- addRow csv row = csv <> [row]
                    addRow csv []
                else
                    -- todo ??????_
                    csv
  pure _csv_

piyo csv char raq riqc rrteq = do
  modify_ (\s -> false) raq -- TODO ref bool afterQuote
  modify_ (\s -> false) rrteq -- TODO ref bool readyToEndQuote

  -- pure char
  -- let csv_ = init csv
  -- let row = tail csv
  -- let str = tail row
  -- let csv_ = case init csv of
  --              Just x -> x
  --              Nothing -> []
  -- let row = case last csv of
  --             Just x -> x
  --             Nothing -> []
  -- let str = case last row of
  --             Just x -> x
  --             Nothing -> ""
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

foo csv char raq riqc rrteq = do
  modify_ (\s -> true) raq -- TODO ref bool afterQuote
  modify_ (\s -> true) rrteq -- TODO ref bool readyToEndQuote
  pure csv

foofoo csv char raq riqc rrteq = do
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

bar :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
bar csv char raq riqc rrteq = do
  -- pure char
  -- let csv_ = case init csv of
  --              Just x -> x
  --              Nothing -> []
  -- let row = case last csv of
  --             Just x -> x
  --             Nothing -> []
  -- let str = case last row of
  --             Just x -> x
  --             Nothing -> ""
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  pure $ addChar csv_ row str char

baz :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect (Array (Array String))
baz csv char raq riqc rrteq = do
  -- let csv_ = init csv
  -- let row = tail csv
  -- let str = tail row
  let csv_ = case init csv of
               Just x -> x
               Nothing -> []
  let lastRow = case last csv of
              Just x -> x
              Nothing -> []
  let row = case init lastRow of
              Just x -> x
              Nothing -> []
  let str = case last lastRow of
              Just x -> x
              Nothing -> ""
  -- let str = case last lastCell of
  --             Just x -> x
  --             Nothing -> ""
  logShow csv_
  logShow row
  log str
  -- if char == delimiterChar then TODO
  -- if char == "\n" then
  --   log "#jsidjfaidiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiif"
  -- else
  --   log char
  let _csv_ =
              if char == "," then
                addCell csv_ (row <> []) str
              else if char == "\n" then
                -- addCell row cell
                -- addCell csv_ row str []
                -- addRow row cell
                addRow (addCell csv_ row str) row
              else if char == "\"" then
                csv
              else
                -- pure char
                -- let csv_ = init csv
                -- let row = tail csv
                -- let str = tail row
                addChar csv_ row "" (str <> char)

  iqc <- read riqc
  if char == "\"" then
    modify_ (\s -> true) riqc -- TODO ref bool insideQuoteCell
  else 
    modify_ (\s -> iqc) riqc -- TODO ref bool insideQuoteCell

  pure _csv_

--   case char of
--     "\"" -> do
--                 "\\\""
--                 "\\\""
--     "1" -> ""
--     "2" -> ""
--     "3" -> ""
--     "4" -> ""
--     "5" -> ""
--     _ -> ""

getAfterQuote :: Ref Boolean -> Effect Boolean
getAfterQuote r = read r

getInsideQuoteCell :: Ref Boolean -> Effect Boolean
getInsideQuoteCell r = read r

getReadyToEndQuote :: Ref Boolean -> Effect Boolean
getReadyToEndQuote r = read r

-- static List<List<string>> Parse(string data, Delimiter delimiter)
-- {
-- 	var sheet = new List<List<string>>();
-- 	var row = new List<string>();
-- 	var cell = new StringBuilder();
-- 	var afterQuote = false;
-- 	var insideQuoteCell = false;
-- 	var readyToEndQuote = false;
-- 	var delimiterChar = Delimiters[delimiter];
-- 
-- 	// TODO : コードパスがひじょーにアレなので見やすく改良
-- 
-- 	ConvertToCrlf(ref data);
-- 
-- 	foreach (var character in data) <- IMACOCO
-- 	{
-- 		// Inside the quotation marks.
-- 		if (insideQuoteCell)
-- 		{
-- 			if (afterQuote)
-- 			{
-- 				                                    if (character == '"')
-- 				                                    {
-- 				                                    	// Consecutive quotes : A quotation mark.
-- 				                                    	cell.Append("\"");
-- 				                                    	afterQuote = false;
-- 				                                    }
-- 				                                    else if (readyToEndQuote && character != '"')
-- 				                                    {
-- 				                                    	// Non-consecutive quotes : End of the quotation.
-- 				                                    	afterQuote = false;
-- 				                                    	insideQuoteCell = false;
-- 
-- 				                                    	if (character == delimiterChar)
-- 				                                    	{
-- 				                                    		AddCell(row, cell);
-- 				                                    	}
-- 				                                    }
-- 				                                    else
-- 				                                    {
-- 				                                    	cell.Append(character);
-- 				                                    	afterQuote = false;
-- 				                                    }
-- 
-- 				                                    readyToEndQuote = false;
-- 			}
-- 			else
-- 			{
-- 				                                    if (character == '"')
-- 				                                    {
-- 				                                    	// A quot mark inside the quotation.
-- 				                                    	// Determine by the next character.
-- 				                                    	afterQuote = true;
-- 				                                    	readyToEndQuote = true;
-- 				                                    }
-- 				                                    else
-- 				                                    {
-- 				                                    	cell.Append(character);
-- 				                                    }
-- 			}
-- 		}
-- 		else
-- 		{
-- 			// Outside the quotation marks.
-- 			if (character == delimiterChar)
-- 			{
-- 				AddCell(row, cell);
-- 			}
-- 			else if (character == '\n')
-- 			{
-- 				AddCell(row, cell);
-- 				AddRow(sheet, ref row);
-- 			}
-- 			else if (character == '"')
-- 			{
-- 				insideQuoteCell = true;
-- 			}
-- 			else
-- 			{
-- 				cell.Append(character);
-- 			}
-- 		}
-- 	}
-- 
-- 	// Add last line except blank line
-- 	if (row.Count != 0 || cell.Length != 0)
-- 	{
-- 		AddCell(row, cell);
-- 		AddRow(sheet, ref row);
-- 	}
-- 
-- 	return sheet;
-- }
-- 
-- static void AddCell(List<string> row, StringBuilder cell)
-- {
-- 	row.Add(cell.ToString());
-- 	cell.Length = 0; // Old C#.
-- }
-- 
-- static void AddRow(List<List<string>> sheet, ref List<string> row)
-- {
-- 	sheet.Add(row);
-- 	row = new List<string>();
-- }
-- 
