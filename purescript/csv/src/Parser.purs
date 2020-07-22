module Parser where

import Prelude

import Effect (Effect)
import Effect.Console (log, clear, logShow, error)
import Node.Stream (Readable, Writable)
import Node.Process (stdin, stdout, argv)
import Data.Function.Uncurried (Fn1, runFn1, Fn4, runFn4)
import Data.Generic.Rep (class Generic)
import Data.Array (replicate, length, head, reverse, filter, snoc, cons)
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

hoge :: String
hoge = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

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
                    Right r -> replace r "\r\n" s

initValue :: Array (Array String)
initValue = []

-- TODO column validate?

-- static readonly Dictionary<Delimiter, char> Delimiters = new Dictionary<Delimiter, char>() {{Delimiter.Comma, ','}, {Delimiter.Tab, '\t'}};
parse :: String -> Delimiter -> Effect Array (Array String)
parse s d = do
  aq <- new false
  iqc <- new false
  rteq <- new false

  foldl (\val -> \char -> readChar val char aq iqx rteq) initValue (split (Pattern "") (convertToCrlf s))

addChar :: Array (Array String) -> Array String -> String -> String -> Array (Array String)
addChar csv row str char = csv <> [row <> [str <> char]]

addCell :: Array (Array String) -> Array String -> String -> Array (Array String)
addCell csv row cell = csv <> [row <> [cell]]

addRow :: Array (Array String) -> Array String -> Array (Array String)
addRow csv row = csv <> [row]

-- createTextRecord txt =  foldl (\x -> \y -> snoc x {row: (length x + 1), char: y}) initTxtRecord (split (Pattern "\n") txt)

-- 次はafterquote とか insidequoteとかごとのcaseを追加？
readChar :: Array (Array String) -> String -> Ref AfterQuote -> Ref InsideQuoteCell -> Ref ReadyToEndQuote -> Effect Array (Array String)
readChar csv char raq riqc rrteq = do
  aq   <- read raq
  iqc  <- read riqc
  rteq <- read rrteq

  csv_ = if char == "\"" && aq == true iqx == true then
         else if
         else
           csv

  pure csv_

  -- XXX TODO ↓のパターンを↑のifに入れる

readChar csv "\"" true true _  = do
  modify_ (\s -> false) srs -- TODO ref bool afterQuote
  modify_ (\s -> false) srs -- TODO ref bool readyToEndQuote

  let csv_ = init csv
  let row = tail csv
  let str = tail row
  addChar csv_ row str char

readChar csv char true true true = do
  if char == "\"" then
    modify_ (\s -> false) srs -- TODO ref bool afterQuote
    modify_ (\s -> false) srs -- TODO ref bool readyToEndQuote

    let csv_ = init csv
    let row = tail csv
    let str = tail row
    addChar csv_ row str char
  else
    modify_ (\s -> false) srs -- TODO ref bool afterQuote
    modify_ (\s -> false) srs -- TODO ref bool insideQuoteCell
    modify_ (\s -> false) srs -- TODO ref bool readyToEndQuote
    if char == delimiterChar then
        -- addCell row cell

        let csv_ = init csv
        let row = tail csv
        let str = tail row
        addCell csv_ row []
    else
        -- todo ??????_
        csv

readChar csv char true true false = do
  modify_ (\s -> false) srs -- TODO ref bool afterQuote
  modify_ (\s -> false) srs -- TODO ref bool readyToEndQuote

  -- pure char
  let csv_ = init csv
  let row = tail csv
  let str = tail row
  addChar csv_ row str char

readChar false _ _ "\"" = do
  modify_ (\s -> true) srs -- TODO ref bool afterQuote
  modify_ (\s -> true) srs -- TODO ref bool readyToEndQuote

readChar false _ _ char = do
  -- pure char
  let csv_ = init csv
  let row = tail csv
  let str = tail row
  addChar csv_ row str char

readChar csv char _ false _ = do
  if char == delimiterChar then
    addCell row cell
  else if char == "\n"
    addCell row cell
    addRow row cell
  else if char == "\""
    modify_ (\s -> true) srs -- TODO ref bool insideQuoteCell
  else
    -- pure char
    let csv_ = init csv
    let row = tail csv
    let str = tail row
    addChar csv_ row str char

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
