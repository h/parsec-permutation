{-# LANGUAGE UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Permutation
-- Copyright   :  (C) 2013 Bitbase, LLC
-- License     :  BSD3
-- Maintainer  :  Samuel Hoffstaetter (samuel@hoffstaetter.com)
-- Stability   :  provisional
-- Portability :  portable
--
-- Text.Parsec.Permutation is a permutation parser for parsec intended as
-- a generalized replacement for Text.Parsec.Perm in parsec.
--
-- Example usage:
--
--   > import Text.Parsec.Permutation
--   >
--   > fooParser :: ParsecT s u m a -> ParsecT s u m [a]
--   > fooParser = runPermParser $
--   >                 (,,) <$> oncePerm (char 'A')
--   >                      <*> manyPerm (char 'B')
--   >                      <*> optionMaybePerm (char 'C' >> char 'D')
--
-- This parser will return ('A', \"BBB\", Just 'D') when parsing for example
-- the strings \"BCDABB\", \"CDBBAB\", &etc.
--
----------------------------------------------------------------------------

module Text.Parsec.Permutation
  (PermParser, runPermParser, runPermParserTill, oncePerm, manyPerm, many1Perm,
   optionPerm, optionMaybePerm)
where

import Control.Monad (void)
import Control.Applicative ((<*>), (<$>), Applicative, pure)
import Text.Parsec ((<|>), ParsecT, Stream, parserZero, optionMaybe, unexpected)

data PermParser s u m a =
  PermParser {
      permValue :: Maybe a -- potential intermediate value parsed so far
    , permParser :: ParsecT s u m (PermParser s u m a)
    }

instance Functor (PermParser s u m) where
  fmap f (PermParser value parser) =
      PermParser (f <$> value) (fmap f <$> parser)

instance Stream s m t => Applicative (PermParser s u m) where
  parser1 <*> parser2 =
      PermParser (permValue parser1 <*> permValue parser2)
                 (attemptParser1 <|> attemptParser2)
    where attemptParser1 = do parser1 <- permParser parser1
                              return $ parser1 <*> parser2
          attemptParser2 = do parser2 <- permParser parser2
                              return $ parser1 <*> parser2

  pure value = PermParser (Just value) parserZero

-- | Turns a permutation parser into a regular parsec parser.
runPermParser :: Stream s m t => PermParser s u m a -> ParsecT s u m a
runPermParser (PermParser value parser) =
    do result <- optionMaybe parser
       case result of
         Nothing -> fromJustOrFail value
         Just permParser -> runPermParser permParser

-- | Similar to runPermParser, but attempts parsing permutations only until the
--   given @untilParser@ succeeds (similar to @manyTill@ in Text.Parsec).
runPermParserTill :: Stream s m t
                  => ParsecT s u m end -> PermParser s u m a -> ParsecT s u m a
runPermParserTill untilParser (PermParser value parser) =
    do void $ untilParser
       fromJustOrFail value
    <|>
    do result <- optionMaybe parser
       case result of
         Nothing -> unexpected "end of permutation parser"
         Just permParser -> runPermParserTill untilParser permParser

-- Similar to "Data.Maybe.fromJust" but fails with an appropriate error message
fromJustOrFail :: Maybe a -> ParsecT s u m a
fromJustOrFail value =
  maybe (fail "Could not parse all permutations") return value

-- | Attempt parsing a value once. Fails if parsing the value succeeds multiple
--   times.
oncePerm :: (Stream s m t) => ParsecT s u m a -> PermParser s u m a
oncePerm parser =
    PermParser Nothing $
      do value <- parser
         return $ PermParser (Just value) $
                    parser >> unexpected "duplicate occurrence.\
                                         \ Expected only one occurrence."

-- | Attempt parsing a value at most once. Fails when parsing the value
--   succeeds multiple times. The first argument is the default value to be
--   used when parsing never succeeds.
optionPerm :: (Stream s m t)
           => a -> ParsecT s u m a -> PermParser s u m a
optionPerm defaultValue parser =
    PermParser (Just defaultValue) $
      do value <- parser
         return $ PermParser (Just value) $
                    parser >> unexpected "duplicate optional occurrence.\
                                         \ Expected at most one occurrence."

-- | Similar to "optionPerm", but uses Nothing as the default value.
optionMaybePerm :: (Stream s m t)
                => ParsecT s u m a -> PermParser s u m (Maybe a)
optionMaybePerm parser = optionPerm Nothing (Just <$> parser)

-- | Parses a given value as many times as possible in the permutation. As with
--   Parsec.Prim.many in parsec, you need to make sure that the provided parser
--   consumes input when succeeding to prevent infinite recursion.
manyPerm :: ParsecT s u m a -> PermParser s u m [a]
manyPerm  parser = manyPermAccum (Just []) parser

-- | Same as "manyPerm", but fails when the parsing doesn't succeed at least
--   once.
many1Perm :: ParsecT s u m a -> PermParser s u m [a]
many1Perm parser = manyPermAccum Nothing   parser

-- helper function for manyPerm / many1Perm
manyPermAccum :: Maybe [a] -> ParsecT s u m a -> PermParser s u m [a]
manyPermAccum accumValue parser =
    PermParser (reverse <$> accumValue) $
      do value <- parser
         let combinedValue = maybe [value] (value:) accumValue
         return $ manyPermAccum (Just combinedValue) parser

