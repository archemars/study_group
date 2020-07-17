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



hoge :: String
hoge = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

-- public enum Delimiter
-- {
-- 	Comma,
-- 	Tab
-- }
type Delimiter = String


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

-- static readonly Dictionary<Delimiter, char> Delimiters = new Dictionary<Delimiter, char>() {{Delimiter.Comma, ','}, {Delimiter.Tab, '\t'}};
parse :: String -> Delimiter -> Array (Array String)
parse s d = foldl (\val -> \char ->  val <> [[char]]) initValue (split (Pattern "") (convertToCrlf s))
-- createTextRecord txt =  foldl (\x -> \y -> snoc x {row: (length x + 1), char: y}) initTxtRecord (split (Pattern "\n") txt)

-- 次はafterquote とか insidequoteとかごとのcaseを追加？
readChar :: String -> String
readChar char = case char of
                  "\"" -> ""
                  "1" -> ""
                  "2" -> ""
                  "3" -> ""
                  "4" -> ""
                  "5" -> ""
                  _ -> ""


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
-- 				if (character == '"')
-- 				{
-- 					// Consecutive quotes : A quotation mark.
-- 					cell.Append("\"");
-- 					afterQuote = false;
-- 				}
-- 				else if (readyToEndQuote && character != '"')
-- 				{
-- 					// Non-consecutive quotes : End of the quotation.
-- 					afterQuote = false;
-- 					insideQuoteCell = false;
-- 
-- 					if (character == delimiterChar)
-- 					{
-- 						AddCell(row, cell);
-- 					}
-- 				}
-- 				else
-- 				{
-- 					cell.Append(character);
-- 					afterQuote = false;
-- 				}
-- 
-- 				readyToEndQuote = false;
-- 			}
-- 			else
-- 			{
-- 				if (character == '"')
-- 				{
-- 					// A quot mark inside the quotation.
-- 					// Determine by the next character.
-- 					afterQuote = true;
-- 					readyToEndQuote = true;
-- 				}
-- 				else
-- 				{
-- 					cell.Append(character);
-- 				}
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
