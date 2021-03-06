module Main where

import Text.XML.HaXml 
import Text.XML.HaXml.Posn (noPos)

contentAs :: (Read t) => (Content i) -> t
contentAs cont = read str
        where (CString _ str _) = cont
              
getList :: String -> [(Float, Float)]              
getList cont = zip xpoints ypoints
        where
          (Document _ _ root _) = xmlParse "error.log" cont
          pointFilter = attr "x" `o` attr "y" `o` (tag "letter" /> tag "point")
          rootElem = CElem root noPos
          xpoints = map (contentAs) (("x" ?) `o` pointFilter $ rootElem)
          ypoints = map (contentAs) (("y" ?) `o` pointFilter $ rootElem)


main = do
    cont <- readFile "test.xml"
    let l = getList cont
    putStrLn (show l)