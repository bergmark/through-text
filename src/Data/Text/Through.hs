{-# LANGUAGE
    FlexibleInstances
  , TypeSynonymInstances
  #-}
module Data.Text.Through
  (
  -- * Type classes
    ToText (..)
  , FromText (..)

  -- * Double conversion
  , throughText

  -- * Types and type aliases
  , LazyByteString
  , LazyText
  , StrictByteString
  , StrictText
  , CI

  -- * Provided for efficiency
  , lazyByteStringToLazyText
  , lazyTextToLazyByteString
  ) where

import Data.CaseInsensitive (CI, FoldCase, mk, original)
import Data.Text.Encoding.Error (lenientDecode)
import qualified Data.ByteString         as StrictByte (ByteString)
import qualified Data.ByteString.Lazy    as LazyByte (ByteString, fromChunks, toChunks)
import qualified Data.Monoid             as M
import qualified Data.Text               as StrictText (Text, pack, unpack)
import qualified Data.Text.Encoding      as StrictText (decodeUtf8With, encodeUtf8)
import qualified Data.Text.Lazy          as LazyText (Text, fromChunks, toChunks)
import qualified Data.Text.Lazy.Encoding as LazyText (decodeUtf8With, encodeUtf8)

-- * Type synonyms

type LazyByteString   = LazyByte.ByteString
type LazyText         = LazyText.Text
type StrictByteString = StrictByte.ByteString
type StrictText       = StrictText.Text

-- * Type classes

class ToText a where
  toText   :: a -> StrictText

class FromText a where
  fromText :: StrictText -> a

-- * Functions

-- | Convert between all textual types.
throughText :: (ToText a, FromText b) => a -> b
throughText = fromText . toText

-- | More efficient than throughText, replaces invalid characters with U+FFFD.
lazyByteStringToLazyText :: LazyByteString -> LazyText
lazyByteStringToLazyText = LazyText.decodeUtf8With lenientDecode

-- | More efficient than throughText
lazyTextToLazyByteString :: LazyText -> LazyByteString
lazyTextToLazyByteString = LazyText.encodeUtf8

-- * Instances

-- | The identity instance is used so 'throughText' works even when converting to 'StrictText'
instance ToText StrictText where
  toText   = id

-- | The identity instance is used so 'throughText' works even when converting to 'StrictText'.
instance FromText StrictText where
  fromText = id

instance ToText [Char] where
  toText   = StrictText.pack

instance FromText [Char] where
  fromText = StrictText.unpack

instance ToText LazyText where
  toText   = M.mconcat . LazyText.toChunks

instance FromText LazyText where
  fromText = LazyText.fromChunks . return

-- | Uses lenient decoding which replaces invalid characters with U+FFFD.
instance ToText StrictByteString where
  toText   = StrictText.decodeUtf8With lenientDecode

instance FromText StrictByteString where
  fromText = StrictText.encodeUtf8

-- | Uses lenient decoding which replaces invalid characters with U+FFFD.
instance ToText LazyByteString where
  toText   = StrictText.decodeUtf8With lenientDecode . M.mconcat . LazyByte.toChunks

instance FromText LazyByteString where
  fromText = LazyByte.fromChunks . return . StrictText.encodeUtf8

instance ToText a => ToText (CI a) where
  toText   = toText . original

instance (FoldCase a, FromText a) => FromText (CI a) where
  fromText = mk . fromText
