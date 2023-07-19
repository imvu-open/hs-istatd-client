{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Imvu.Network.IstatdClient.Types
    ( Name
    , Packet (..)
    , PacketType (..)
    , nameFromBytes
    , bytesFromName
    ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import Data.String (IsString (fromString))
import Data.Word (Word8)
import GHC.Generics (Generic)

import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NonEmpty

newtype Name = Name { bytesFromName :: BS.ByteString }
               deriving (Show, Eq, Ord, Generic, NFData)

data PacketType = Increment | Record
    deriving (Show, Generic)

data Packet = Packet
    { packetType     :: !PacketType
    , packetName     :: !Name
    , packetSuffixes :: ![Name]
    , packetValue    :: !Double
    } deriving (Show, Generic)

instance NFData Packet
instance NFData PacketType

instance Hashable Name

instance IsString Name where
    fromString str = case nameFromBytes $ fromString str of
        Just name -> name
        Nothing -> error $ "counter name " ++ show str ++ " is invalid!"
    {-# INLINE fromString #-}

instance Semigroup Name where
    Name a <> Name b = Name $ BS.concat [a, ".", b]
    {-# INLINE (<>) #-}
    sconcat = Name . BS.intercalate "." . map bytesFromName . NonEmpty.toList
    {-# INLINE sconcat #-}

nameFromBytes :: BS.ByteString -> Maybe Name
nameFromBytes bytes = if valid then Just $ Name bytes else Nothing
  where
    valid = BS.all validByte bytes &&
        not adjacentPeriods &&
        not leadingPeriod &&
        not trailingPeriod
    validByte byte =
        isAsciiPeriod byte ||
        isAsciiAlphaNum byte ||
        isAsciiSeparator byte
    adjacentPeriods = ".." `BS.isInfixOf` bytes
    (leadingPeriod, trailingPeriod) = if not $ BS.null bytes
        then (asciiPeriod == BS.head bytes, asciiPeriod == BS.last bytes)
        else (False, False)
{-# INLINE nameFromBytes #-}

asciiPeriod :: Word8
asciiPeriod = 46

isAsciiPeriod :: Word8 -> Bool
isAsciiPeriod = (== asciiPeriod)
{-# INLINE isAsciiPeriod #-}

isAsciiSeparator :: Word8 -> Bool
isAsciiSeparator byte =
    byte == 95 || -- underscore
    byte == 45 -- hyphen-minus
{-# INLINE isAsciiSeparator #-}

isAsciiAlphaNum :: Word8 -> Bool
isAsciiAlphaNum byte =
    (byte >= 65 && byte <= 90) || -- A-Z
    (byte >= 97 && byte <= 122) || -- a-z
    (byte >= 48 && byte <= 57) -- 0-9
{-# INLINE isAsciiAlphaNum #-}
