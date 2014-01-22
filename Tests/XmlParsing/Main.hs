module Main where

import Text.XML.HaXml
import Text.XML.HaXml.Posn

-- http://www.kennknowles.com/blog/2008/04/20/using-haxml-to-make-a-pdf-slideshow-from-an-inkscape-svg/
-- http://www.rsdn.ru/article/haskell/haskell_xml.xml



contentAs :: (Content i) -> String
contentAs (CString _ str _) = str

attributes = [ "events"
             , "output"
             , "filename"
             , "generators"
             , "limit"
             , "format"
             ]
             
--attribFilter = foldr keep attributes

-- o :: CFilter i -> CFilter i -> CFilter i
-- a, b :: CFilter i 

flt = attr "events" `o` attr "output"

-- foldr :: (a -> b -> b) -> b -> [a] -> b
translate :: Document Text.XML.HaXml.Posn.Posn -> [(String, [String])]
translate (Document _ _ root _) = let
    rootElem = CElem root noPos
   
    logTag = tag "logmap" /> tag "log"
    
    attributesExtractor a = ((iffind a literal none) `o` logTag) rootElem
    thisArgData a = (a, map contentAs (attributesExtractor a))
    allArgsData = map thisArgData attributes
                                  in allArgsData



main = do
    
    content <- readFile "log.xml"
    
    let xmlDoc = xmlParse "error.log" content 
    
    let myStruct = translate xmlDoc
    putStrLn (show myStruct)
    
    putStrLn "All Ok."