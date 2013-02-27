{-# LANGUAGE TemplateHaskell, TupleSections, FlexibleContexts #-}
module Main where

import System.Exit
import Test.QuickCheck
import Test.QuickCheck.All
import Text.Parsec
import Control.Applicative
import Data.List

import Text.Parsec.Permutation

requireParse :: String -> Parsec String () a -> a
requireParse str parser =
  case parse (parser <* eof) str str of
       Left err -> error $ "Unsuccessful parse: " ++ show err
       Right value -> value

prop_purePerm :: Property
prop_purePerm = once $
  (requireParse "" $ runPermParser $ pure "test") == "test"

prop_purePerm2 :: Property
prop_purePerm2 = once $
  case parse (runPermParser $ pure (, "asdf") <*> pure "qwer") "" "" of
       Left _ -> False
       Right x -> x == ("qwer", "asdf")

prop_oncePermTill :: Property
prop_oncePermTill = once $
  (requireParse "sdfxa" $ (runPermParserTill (char 'x') $
    (,,,,,) <$> pure 'n'
            <*> oncePerm (char 's')
            <*> oncePerm (char 'f')
            <*> oncePerm (char 'd')
            <*> optionMaybePerm (char 'a')
            <*> optionMaybePerm (char 'x')) <* char 'a')
      == ('n','s','f','d',Nothing,Nothing)

prop_oncePermTill2 :: Property
prop_oncePermTill2 = once $
  (requireParse "sdfax" $ runPermParserTill (char 'x') $
    (,,,,,) <$> pure 'n'
            <*> oncePerm (char 's')
            <*> oncePerm (char 'f')
            <*> oncePerm (char 'd')
            <*> optionMaybePerm (char 'a')
            <*> optionMaybePerm (char 'x')) == ('n','s','f','d',Just 'a',Nothing)

prop_oncePerm :: Property
prop_oncePerm = once $
  case parse (runPermParser $ oncePerm $ char 'A') "" "A" of
       Left _ -> False
       Right x -> x == 'A'

prop_oncePerm2 :: Property
prop_oncePerm2 = once $
  (requireParse "asdf" $ runPermParser $
     (,,,,) <$> pure 'x'
            <*> oncePerm (char 's')
            <*> oncePerm (char 'a')
            <*> oncePerm (char 'f')
            <*> oncePerm (char 'd')) == ('x','s','a','f','d')

prop_oncePerm3 :: Property
prop_oncePerm3 = once $
  (requireParse "a" $ runPermParser $
     (,) <$> pure 'x'
         <*> oncePerm (char 'a')) == ('x','a')

prop_oncePerm4 :: Property
prop_oncePerm4 = once $
  (requireParse "as" $ runPermParser $
    (,,) <$> pure 'x'
         <*> oncePerm (char 's')
         <*> oncePerm (char 'a')) == ('x','s','a')

prop_oncePerm5 :: Property
prop_oncePerm5 = once $
  (requireParse "asf" $ runPermParser $
   (,,,) <$> pure 'x'
         <*> oncePerm (char 's')
         <*> oncePerm (char 'a')
         <*> oncePerm (char 'f')) == ('x','s','a','f')

prop_oncePerm6 :: Property
prop_oncePerm6 = once $
  (requireParse "ab" $ runPermParser $
     oncePerm (char 'a' >> return ('a',)) <*>
     oncePerm (char 'b')) == ('a','b')

prop_oncePerm7 :: Property
prop_oncePerm7 = once $
  (requireParse "ba" $ runPermParser $
     oncePerm (char 'a' >> return ('a',)) <*>
     oncePerm (char 'b')) == ('a','b')

prop_oncePerm8 :: Property
prop_oncePerm8 = once $
  (requireParse "ba" $ runPermParser $
     oncePerm (char 'a' >> return ('a',)) <*>
     (oncePerm (char 'b' >> return ('b',)) <*> pure 'c')) == ('a',('b','c'))

prop_oncePerm9 :: Property
prop_oncePerm9 = once $
  (requireParse "bac" $ runPermParser $
     oncePerm (char 'a' >> return ('a',)) <*>
     (oncePerm (char 'b' >> return ('b',)) <*>
      oncePerm (char 'c'))) == ('a',('b','c'))

prop_oncePerm10 :: Property
prop_oncePerm10 = once $
  (requireParse "" $ runPermParser $
     pure ('a',) <*>
     (pure ('b',) <*> pure 'c')) == ('a',('b','c'))

prop_oncePermAll' :: Property
prop_oncePermAll' = once $ all helper ["", "a", "ab", "bb", "abc", "abcb",
                                       "abcdefg", "abccefaoeifjalasdfie"] where
 helper str =
  (requireParse str $ runPermParser $
      foldr (\c parser ->
             oncePerm (char c >> return (c:)) <*> parser)
            (pure "Q") str
  ) == str ++ "Q"

prop_oncePermAll :: String -> Property
prop_oncePermAll str = property $
  (requireParse (nub $ sort str) $ runPermParser $
      foldr (\c parser ->
             oncePerm (char c >> return (c:)) <*> parser)
            (pure "Q") (nub $ sort str)
  ) == (nub $ sort str) ++ "Q"

prop_oncePermAllReverse :: String -> Property
prop_oncePermAllReverse str = property $
  (requireParse (reverse $ nub $ sort str) $ runPermParser $
      foldr (\c parser ->
             oncePerm (char c >> return (c:)) <*> parser)
            (pure "Q") (nub $ sort str)
  ) == (nub $ sort str) ++ "Q"

prop_oncePermMixedOrder1 :: Property
prop_oncePermMixedOrder1 = once $ expectFailure $
  (requireParse "abcc" $ runPermParser $
     oncePerm (char 'c' >> return ('c',,)) <*>
        (oncePerm (char 'b' >> return ('b',)) <*>
         oncePerm (char 'c')) <*>
     oncePerm (char 'a')) == ('c',('b','c'),'a')

prop_oncePermMixedOrder1' :: Property
prop_oncePermMixedOrder1' = once $ expectFailure $
  (requireParse "abc" $ runPermParser $
     oncePerm (char 'c' >> return ('c',,)) <*>
     oncePerm (char 'b') <*>
     oncePerm (char 'a')) == ('c','b','a')

prop_oncePermMixedOrder2 :: Property
prop_oncePermMixedOrder2 = once $ expectFailure $
  (requireParse "abbc" $ runPermParser $
     oncePerm (char 'c' >> return ('c',,)) <*>
        (oncePerm (char 'b' >> return ('b',)) <*>
         oncePerm (char 'c')) <*>
     oncePerm (char 'a')) == ('c',('b','c'),'a')

prop_oncePermMixedOrder3 :: Property
prop_oncePermMixedOrder3 = once $
  (requireParse "axcb" $ runPermParser $
     oncePerm (char 'b' >> return ('b',,)) <*>
        (oncePerm (char 'x' >> return ('x',)) <*>
         oncePerm (char 'c')) <*>
     oncePerm (char 'a')) == ('b',('x','c'),'a')

prop_manyPermMixedOrder1 :: Property
prop_manyPermMixedOrder1 = once $
  (requireParse "abcc" $ runPermParser $
     (,,) <$>
     manyPerm (char 'c') <*>
        ((,) <$>
         manyPerm (char 'b') <*>
         manyPerm (char 'c')) <*>
     manyPerm (char 'a')) == ("cc",("b",""),"a")

prop_manyPermMixedOrder1' :: Property
prop_manyPermMixedOrder1' = once $ expectFailure $
  (requireParse "abcc" $ runPermParser $
     (,,) <$>
     manyPerm (char 'c') <*>
        ((,) <$>
         manyPerm (char 'b') <*>
         many1Perm (char 'c')) <*>
     manyPerm (char 'a')) == ("cc",("b",""),"a")

prop_manyPermMixedOrder2 :: Property
prop_manyPermMixedOrder2 = once $
  (requireParse "abbc" $ runPermParser $
     (,,) <$>
     manyPerm (char 'c') <*>
        ((,) <$>
         manyPerm (char 'b') <*>
         manyPerm (char 'c')) <*>
     manyPerm (char 'a')) == ("c",("bb",""),"a")

prop_manyPermMixedOrder3 :: Property
prop_manyPermMixedOrder3 = once $
  (requireParse "abcb" $ runPermParser $
     (,,) <$>
     manyPerm (char 'b') <*>
        ((,) <$>
         manyPerm (char 'b') <*>
         manyPerm (char 'c')) <*>
     manyPerm (char 'a')) == ("bb",("","c"),"a")

prop_manyPermMixedOrder4 :: Property
prop_manyPermMixedOrder4 = once $ expectFailure $
  (requireParse "abcbxb" $ runPermParser $
     (,,) <$>
     oncePerm (char 'x') <*>
        ((,) <$>
         optionMaybePerm (char 'b') <*>
         manyPerm (anyChar)) <*>
     manyPerm (char 'a')) /= ('q',(Just 'q',"qqq"),"qq")

prop_manyPermMixedOrder4' :: Property
prop_manyPermMixedOrder4' = once $
  (requireParse "abczxb" $ runPermParser $
     (,,) <$>
     oncePerm (char 'x') <*>
        ((,) <$>
         optionMaybePerm (char 'z') <*>
         manyPerm (anyChar)) <*>
     manyPerm (char 'a')) == ('x',(Just 'z',"cbb"),"a")

prop_manyPermAnyChar :: Property
prop_manyPermAnyChar = once $
  (requireParse "abcbdef" $ runPermParser $
     (,,) <$>
     manyPerm (char 'b') <*>
     manyPerm (anyChar) <*>
     manyPerm (anyChar)) == ("bb","acdef","")

prop_optionPermAll :: String -> String -> Property
prop_optionPermAll str' optionalStr = property $
  whenFail (putStrLn $ "str: " ++ show str) $
  whenFail (putStrLn $ "optional: " ++ show optionalStr) $
  whenFail (putStrLn $ "remainder: " ++ show (str \\ optionalStr)) $
  whenFail (putStrLn $ "values: " ++ show ((map snd $
         tail $ scanl (\(str, _) c ->
                       if c `elem` str
                       then (str \\ [c], Just c)
                       else (str, Nothing)) (str, Just 'Q')
                optionalStr) ++ [Just 'Q'])) $
  whenFail (putStrLn $ "actual: " ++ show (requireParse str $ runPermParser $
      (,) <$>
      foldr (\c parser ->
             (:) <$> optionMaybePerm (char c) <*> parser)
            (pure $ [Just 'Q']) optionalStr <*>
      manyPerm anyChar)) $
  (requireParse str $ runPermParser $
      (,) <$>
      foldr (\c parser ->
             (:) <$> optionMaybePerm (char c) <*> parser)
            (pure $ [Just 'Q']) optionalStr <*>
      manyPerm anyChar
  ) == ( (map snd $
          tail $ scanl (\(str, _) c ->
                        if c `elem` str
                        then (str \\ [c], Just c)
                        else (str, Nothing)) (str, Just 'Q')
                 optionalStr) ++ [Just 'Q']
        , str \\ optionalStr)
  where str = nub $ sort str'

main :: IO ()
main = do
  success <- $quickCheckAll
  if success then exitSuccess else exitFailure

