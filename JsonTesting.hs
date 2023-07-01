module JsonTesting where

import JsonParsers

import Test.QuickCheck

checkEmptyDict = runParser jsonObjParser "{}" === Just ("", JsonDict [])
checkEmptyList = runParser jsonObjParser "[]" === Just ("", JsonList [])

checkIntList :: [Int] -> Property
checkIntList as = runParser jsonObjParser (show as) === Just ("", JsonList (map JsonInt as))