module Main where

import qualified Data.List as L
import qualified Data.ByteString.Char8 as C

englishLetters = C.pack $ "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++ "abcdefghijklmnopqrstuvwxyz"
--rusLetters = C.pack $ "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ" ++ "абвгдеёжзийклмнопрстуфхцчшщъыьэюя"
space = C.pack " "
letters = C.concat [englishLetters, space]

lettersCount :: C.ByteString -> (Int, Int)
lettersCount text = let
    textLen = C.length text
    in (lettersCount' 0 text, textLen)
  where
    lettersCount' cnt txt | C.null txt = cnt
                          | otherwise = let
        (symb, rest) = C.splitAt 1 txt
        in if symb `C.isInfixOf` letters
            then lettersCount' (cnt + 1) rest
            else lettersCount' cnt rest
            
lettersCount1 = fst . lettersCount

popSymbol = C.take 1
symbolBalance outS inS | isFirst && isSecond = 0
                       | isFirst && (not isSecond) = (-1)
                       | (not isFirst) && isSecond = 1
                       | otherwise = 0
  where
    isFirst = outS `C.isInfixOf` letters
    isSecond = inS `C.isInfixOf` letters
                    
                    
slide (pointer1, pointer2) res@(s, lCount) | C.null pointer2 = [res]
                                           | otherwise = let
    outSymbol = C.take 1 pointer1
    inSymbol = C.take 1 pointer2
    newLettersCount = lCount + (symbolBalance outSymbol inSymbol)
    in res : slide (C.drop 1 pointer1, C.drop 1 pointer2) (outSymbol, newLettersCount)

letterMap wnd text = let
    t@(starter, pointer2) = C.splitAt wnd text
    pointer1 = C.drop 1 text
    firstSymbol = C.take 1 text
    fitstWindowLettersCount = lettersCount1 starter
    in if (C.length starter < wnd)
       then []
       else slide (pointer1, pointer2) (firstSymbol, fitstWindowLettersCount)
    
divide l t = (fromIntegral l) / (fromIntegral t)

escape symbol | symbol == C.singleton '\n' = C.pack "<br />"
              | otherwise = symbol

ratioTaggedMap wnd lMap = map f lMap
  where
    f (symbol, cnt) = let
        ratio = (divide cnt wnd) * 100.0
        strRatio1 = if ratio > 30.0 then "100"
                                   else take 2 (show ratio)
        strRatio2 = take 2 (show ratio)
        tagPrefix = C.pack $ "<span style=\"background: rgb(" ++ strRatio2 ++ "%,0%,0%);\">"
        tagPostfix = C.pack $ "</span>"
        escapedSymbol = escape symbol
      in C.concat [tagPrefix, escapedSymbol, tagPostfix]
    
htmlize t = C.concat [htmlPrefix, t, htmlPostfix]
  where
    htmlPrefix = C.pack "<html><body>"
    htmlPostfix = C.pack "</body></html>"
    
main = do
    --let testText = "ABC2@#%#$^#$&f"
    fileCont <- C.readFile "text.txt"
    let (lCount, total) = lettersCount fileCont
    putStrLn $ "Letters count: " ++ show lCount
    putStrLn $ "Total count: " ++ show total
    putStrLn $ "Ratio = " ++ (show $ divide lCount total)
    
    let wnd = 10
    let lMap = letterMap wnd fileCont
    let tagged = C.concat $ ratioTaggedMap wnd lMap
    let html = htmlize tagged
    C.writeFile "taggedSymbols4.html" html 
    putStrLn "Ok."