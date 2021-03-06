module Main where

import Text.Parsec.String
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec

import Text.XML.Generator
import qualified Data.ByteString as BS
import Data.Char

import Text.XML.HaXml
import Text.XML.HaXml.Posn

intString = "10"
notIntString = "Bob20Lion"
personString = "Bob20Lion"
notPerson = "#%@%#%@#%#@%"

parseInt :: GenParser Char st Int
parseInt = do
   spaces
   res <- many1 digit
   return (read res)

test1 :: Either ParseError Int
test1 = parse parseInt [] "   10"

test2 :: Either ParseError Int
test2 = parse parseInt [] "AAA"

data Person = Person
            { name :: String
            , age :: Int
            , zodiac :: String }
    deriving (Show)

parsePerson' :: GenParser Char st (String, Int, String)
parsePerson' = do
    name <- parseName
    age <- parseInt
    zodiac <- parseName
    return (name, age, zodiac)

parsePerson :: GenParser Char st Person
parsePerson = do
    name <- many1 letter <?> "Name"
    age <- parseInt <?> "Age"
    zodiac <- many1 letter <?> "Zodiac"
    return (Person name age zodiac)

test s = parse parsePerson "" s
t = parse parsePerson "" notPerson

parseName :: GenParser Char st String
parseName = do
    spaces
    first <- upper
    rest <- many letter
    return (first : rest)


writePerson :: Person -> Xml Elem
writePerson (Person name age zodiac) = let
    attribs = xattr "Name" name
           <> xattr "Age" (show age)
    info = xelems [ xelem "Zodiac" zodiac ]
    in xelem "Person" ( attribs <#> info )

writePersonXml :: IO ()
writePersonXml = do
    let (Right person) = parse parsePerson "" personString
    let xmlElement = writePerson person
    let xmlStr = xrender $ doc defaultDocInfo xmlElement
    BS.writeFile "Person.xml" xmlStr



readPersonXml :: IO ()
readPersonXml = do
    content <- readFile "Person.xml"
    let (Document _ _ root _) = xmlParse "error.log" content
    let person = toPerson (CElem root noPos)
    print person

toPerson :: Content i -> Person
toPerson p = let
    name = singleAttribute p "Name" id
    age = singleAttribute p "Age" readString
    zodiacF = tag "Person" /> tag "Zodiac"
    zodiac = concatMap txtContent (zodiacF p)
    in Person name age zodiac

readString :: (Read a) => String -> a
readString str = read str

filterTextContent [] = []
filterTextContent (e@(CString _ _ _):rest) = e : filterTextContent rest
filterTextContent (_:rest) = filterTextContent rest

txtContent :: Content i -> String
txtContent parent =
    case filterTextContent $ children parent of
        (CString _ str _:_) -> str
        _ -> error "Not a txt content found."
    
singleAttribute :: Content i -> String -> (String -> a) -> a
singleAttribute tagElement attribute converter =
    case optionalAttribute tagElement attribute converter of
        Nothing -> error $ "Attribute " ++ attribute ++ " is absent."
        Just x -> x
        
optionalAttribute :: Content i -> String -> (String -> a) -> Maybe a
optionalAttribute tagElement attribute converter = let
    extractor = (iffind attribute literal none) tagElement
    items = map stringContent extractor
    in case null items of
        True -> Nothing
        False -> Just . converter . head $ items
        
stringContent :: Content i -> String
stringContent (CString _ str _) = str