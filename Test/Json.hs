module Test.Json where

import Parsers.Json
import Test.QuickCheck

checkEmptyDict = runParser jsonObjP "{}" === Just ("", JsonDict [])

checkEmptyList = runParser jsonObjP "[]" === Just ("", JsonList [])

checkIntList :: [Int] -> Property
checkIntList as = runParser jsonObjP (show as) === Just ("", JsonList (map JsonInt as))

checkFloatList :: [Float] -> Property
checkFloatList as = runParser jsonObjP (show as) === Just ("", JsonList (map JsonFloat as))

checkStringList :: [String] -> Property
checkStringList as = runParser jsonObjP (show as) === Just ("", JsonList (map JsonString as))

checkNestedIntList :: [[Int]] -> Property
checkNestedIntList as = runParser jsonObjP (show as) === Just ("", JsonList (map (JsonList . map JsonInt) as))