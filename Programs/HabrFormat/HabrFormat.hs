import Data.Char
import Data.List
import Text.Printf

type Result = String
type Rest = String
type Prefix = String
type Prefixes = [String]
type FormatData = (Result, Rest)
type Formatter = FormatData -> Maybe FormatData


testText = ([], "<blockquote>printSqrt2&nbsp;x&nbsp;<font color=\"#339933\">=</font>&nbsp;<font>case</font>&nbsp;x&nbsp;<font color=\"#339933\">&#60;</font>&nbsp;<font>0</font>&nbsp;<font>of</font><br/>\n&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;True&nbsp;<font color=\"#339933\">-&#62;</font>&nbsp;<font>putStrLn</font>&nbsp;<font>\"x&nbsp;&#60;&nbsp;0!\"</font><br/>\n&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;False&nbsp;<font color=\"#339933\">-&#62;</font>&nbsp;<font>putStrLn</font>&nbsp;<font>&#40;</font><font>\"Sqrt&nbsp;of&nbsp;\"</font>&nbsp;<font color=\"#339933\">++</font>&nbsp;<font>show</font>&nbsp;x&nbsp;<font color=\"#339933\">++</font>&nbsp;<font>\"&nbsp;=&nbsp;\"</font>&nbsp;<font color=\"#339933\">++</font>&nbsp;<font>show</font>&nbsp;<font>&#40;</font><font>sqrt</font>&nbsp;x<font>&#41;</font><font>&#41;</font><br/>\n&nbsp;<br/>\n<font color=\"#339933\">*</font>Main>"
			++ "<font color=\"#339933\">&#62;</font>&nbsp;:r<br/>\n<font>&#91;</font><font>1</font>&nbsp;<font>of</font>&nbsp;<font>1</font><font>&#93;</font>&nbsp;Compiling&nbsp;Main&nbsp;&nbsp;&nbsp;&nbsp;<font>&#40;</font>H:\\Haskell\\QuestTutorial\\Quest\\QuestMain<font color=\"#339933\">.</font>hs"
			++ "<font color=\"#339933\">,</font>&nbsp;interpreted<font>&#41;</font><br/>\n&nbsp;<br/>\nH:\\Haskell\\QuestTutorial\\Quest\\QuestMain<font color=\"#339933\">.</font>hs:<font>14</font>:<font>23</font>:<br/>\n&nbsp;&nbsp;&nbsp;&nbsp;paerse&nbsp;<font>error</font>&nbsp;on&nbsp;input&nbsp;'<font color=\"#339933\">-&#62;</font>'<br/>\nFailed<font color=\"#339933\">,</font>&nbsp;modules&nbsp;loaded:&nbsp;none<font color=\"#339933\">.</font></blockquote"
			++ "sdfsdf asd fae fe <font color=\"#339933\">=</font>s s fsdf <font>case</font>&nbsp;locNumber<font>\"You&nbsp;are&nbsp;standing&nbsp;in&nbsp;the&nbsp;middle&nbsp;room&nbsp;at&nbsp;the&nbsp;wooden&nbsp;table.\"</font> hh<font>1</font><font>2</font><font>   1.43 </font><br/> <font color=\"#5d478b\">{-&nbsp;Здесь&nbsp;вставлять&nbsp;описание&nbsp;других&nbsp;локаций.<br/> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Или&nbsp;здесь.&nbsp;<br/> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;adfsdf&nbsp;few&nbsp;fef&nbsp;jel&nbsp;jle&nbsp;jkjlefjaiejeo&nbsp;-}</font><font>otherwise</font>&nbsp;<font color=\"#339933\">-&#62;</font>&nbsp;<font>\"Unknown&nbsp;location.\"</font>")

test = writeFile "test.html" (toHabrFormat testText)

libraryLink = printf "\"http://haskell.org/ghc/docs/latest/html/libraries/base/%s.html\""
preludeTypeLink = printf "\"http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#t:%s\""

aHrefBegin :: String -> String
aHrefBegin = printf "<a href=%s>"
aHrefEnd = "</a>"


			
strippedPrefix :: Prefix -> Prefix -> FormatData -> Maybe FormatData
strippedPrefix oldPref newPref a@(formatted, unformatted) = case stripPrefix oldPref unformatted of
							Just restStr -> Just (formatted ++ newPref, restStr)
							Nothing -> Nothing

splitBefore str splitter = takeInternalString ([], str)
	where
		takeInternalString (_, []) = Nothing
		takeInternalString (internalStr, restStr@(r:rs)) = case stripPrefix splitter restStr of
														Just otherStr -> Just (internalStr, otherStr)
														Nothing -> takeInternalString (internalStr ++ [r], rs)

keywords = reverse $ sortBy (\x y -> compare (length x) (length y)) ["as", "*Main>", "case", "of", "class", "data", "default", "deriving",
				"do", "forall", "hiding", "if", "then", "else", "import", "infix",
				"infixl", "infixr", "instance", "let", "in", "module", "newtype", "qualified", "type", "where"]

preludeTypes = reverse $ sortBy (\x y -> compare (length x) (length y)) ["Bool", "Floating", "Maybe", "Either", "Ord", "Ordering", "Char",
	"String", "Eq", "Enum", "Bounded",
    "Int", "Integer", "Float", "Double", "Rational", "Num", "Real", "Integral", "Fractional",
    "RealFrac", "RealFloat", "Monad", "Functor", "Show", "ShowS", "Read", "ReadS", "IO"]

libraries = ["Foreign", "Numeric", "Prelude"]

-- formatter :: Formatter
takePrefix :: String -> Prefixes -> Maybe (Prefix, Rest)
takePrefix _ [] = Nothing
takePrefix [] _ = Nothing
takePrefix str (s:ss) = let y = stripPrefix s str in
					case y of 
						Just rest -> Just (s, rest)
						Nothing -> takePrefix str ss

keywordF (s, r) = case stripPrefix "<font>" r of
					Nothing -> Nothing
					Just rest -> case takePrefix rest keywords of
									Just (pref, rest) -> Just (s ++ "<font color=\"#06c\">" ++ pref ++ "</font>", drop 7 rest)
									Nothing -> Nothing
								
preludeTypeF' (s, r) = case stripPrefix "<font color=\"#cccc00\">" r of
					Nothing -> Nothing
					Just rest -> case takePrefix rest preludeTypes of
									Just (pref, rest) -> Just (s ++ "<font color=green>" ++ (aHrefBegin $ preludeTypeLink pref) ++ pref ++ aHrefEnd ++ "</font>", drop 7 rest)
									Nothing -> Nothing

libraryF (s, r) = case stripPrefix "<font>" r of
					Nothing -> Nothing
					Just rest -> case takePrefix rest preludeTypes of
									Just (pref, rest) -> Just (s ++ "<font color=\"#06c\">" ++ (aHrefBegin $ libraryLink pref) ++ pref ++ aHrefEnd ++ "</font>", drop 7 rest)
									Nothing -> Nothing

									
equalSignF = strippedPrefix "<font color=\"#339933\">=</font>" "<font color=\"#66cc66\"><b>=</b></font>"
--ofKeywordF = strippedPrefix "<font>of</font>" "<font color=\"#06c\">of</font>"
rArrowF    = strippedPrefix "<font color=\"#339933\">-&#62;</font>" "<font color=\"#66cc66\"><b>-&#62;</b></font>"
lArrowF    = strippedPrefix "<font color=\"#339933\">&#60;-</font>" "<font color=\"#66cc66\"><b>&#60;-</b></font>"
lBracketF  = strippedPrefix "<font color=\"#339933\">&#40;</font>" "<font color=\"#66cc66\"><b>&#40;</b></font>"
rBracketF  = strippedPrefix "<font color=\"#339933\">&#41;</font>" "<font color=\"#66cc66\"><b>&#41;</b></font>"
doubleColonF = strippedPrefix "<font color=\"#339933\">::</font>" "<font color=\"#66cc66\"><b>::</b></font>"
colonF       = strippedPrefix "<font color=\"#339933\">:</font>" "<font color=\"#66cc66\"><b>:</b></font>"
otherwiseF = strippedPrefix "otherwise" "<a href=\"http://haskell.org/ghc/docs/latest/html/libraries/base/Prelude.html#v:otherwise\">otherwise</a>"
pipeSignF  = strippedPrefix "<font color=\"#339933\">|</font>" "<font color=\"#66cc66\"><b>|</b></font>"
-- Знаки: ++ || && | & -

stringF (s, r) = case stripPrefix "<font>\"" r of
						Just rest -> case splitBefore rest "\"</font>" of
										Just (internalStr, otherStr) -> Just (s ++ "<i><font color=\"#808080\">\"" ++ internalStr ++ "\"</font></i>", otherStr)
										Nothing -> Nothing
						Nothing -> Nothing

numberF (s, r) = case stripPrefix "<font>" r of
						Just rest -> case splitBefore rest "</font>" of
										Just (internalStr, otherStr) -> case reads internalStr :: [(Float, String)] of
																			[(x, _)] -> Just (s ++ "<font color=red>" ++ internalStr ++ "</font>", otherStr)
																			_ -> Nothing
										Nothing -> Nothing
						Nothing -> Nothing
						
commentF (s, r) = case stripPrefix "<font color=\"#5d478b\">{-" r of
						Just rest -> case splitBefore rest "-}</font>" of
										Just (internalStr, otherStr) -> Just (s ++ "<i><font color=\"#5d478b\">{-" ++ internalStr ++ "-}</font></i>", otherStr)
										Nothing -> Nothing
						Nothing -> Nothing

						
(<|>) :: Formatter -> Formatter -> Formatter
a <|> b = \formatData -> case a formatData of
					Just formatted -> Just formatted
					Nothing -> b formatData

formatters = [equalSignF, keywordF, libraryF, preludeTypeF', otherwiseF, rArrowF, lArrowF, lBracketF, rBracketF, numberF, doubleColonF, colonF, stringF, commentF]



toHabrFormat (s, (ch:[])) = s ++ [ch]
toHabrFormat a@(s, b@(ch:inStr)) = let formatted = (foldr1 (<|>) formatters) a
						in case formatted of
							Just (res, []) -> res
							Just (res, (r:rs)) -> toHabrFormat (res ++ [r], rs)
							Nothing -> toHabrFormat (s ++ [ch], inStr)


main :: IO ()
main = do
	str <- readFile "article.html"
	let res = toHabrFormat ([], str)
	writeFile "formated_article.html" res
	putStrLn ""