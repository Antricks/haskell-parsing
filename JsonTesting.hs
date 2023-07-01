module JsonTesting where

import JsonParsers

import Test.QuickCheck

checkEmptyDict = runParser jsonObjParser "{}" === Just ("", JsonDict [])
checkEmptyList = runParser jsonObjParser "[]" === Just ("", JsonList [])

checkIntList :: [Int] -> Property
checkIntList as = runParser jsonObjParser (show as) === Just ("", JsonList (map JsonInt as))

checkFloatList :: [Float] -> Property
checkFloatList as = runParser jsonObjParser (show as) === Just ("", JsonList (map JsonFloat as))

checkStringList :: [String] -> Property
checkStringList as = runParser jsonObjParser (show as) === Just ("", JsonList (map JsonString as))

checkNestedIntList :: [[Int]] -> Property
checkNestedIntList as = runParser jsonObjParser (show as) === Just ("", JsonList (map (JsonList . map JsonInt) as))