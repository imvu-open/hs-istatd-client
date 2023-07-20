{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | A simple, no-frills istatd client.
module Imvu.Network.IstatdClient
    ( Connection
    , Name
    , Packet (..)
    , PacketType (..)
    , nameFromBytes
    , bytesFromName
    , connect
    , connectLocalhost
    , send
    , sendMany
    , encode
    , close
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Builder (Builder, byteString, char7,
                                toLazyByteString)
import Data.List.Split (chunksOf)

import Imvu.Network.IstatdClient.Types (Name, Packet (..), PacketType (..),
                                        bytesFromName, nameFromBytes)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Double.Conversion.ByteString as PrintDouble
import qualified Network.BSD as NetBsd
import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetBS

data Connection = Connection !Net.Socket !Net.SockAddr

close :: Connection -> IO ()
close (Connection n _) = Net.close n

-- | Opens a UDP connection to an istatd host.
connect :: MonadIO m => Net.HostName -> Net.PortNumber -> m Connection
connect hostName hostPort = liftIO $ do
    hostEntry <- NetBsd.getHostByName hostName
    let addr = Net.SockAddrInet hostPort (NetBsd.hostAddress hostEntry)
    socket <- Net.socket Net.AF_INET Net.Datagram 0
    return $ Connection socket addr

-- | Opens a UDP connection to an istatd host listening on localhost:8111.
connectLocalhost :: MonadIO m => m Connection
connectLocalhost = connect "localhost" 8111

-- | Serializes an istatd packet as a ByteString.
encode :: Packet -> Builder
encode (Packet ptype name suffixes value) =
    prefix <>
    byteString (bytesFromName name) <>
    foldMap (\x -> char7 '^' <> byteString (bytesFromName x)) suffixes <>
    char7 ' ' <>
    byteString (PrintDouble.toShortest value) <>
    char7 '\n'
  where
    prefix = case ptype of
        Increment -> char7 '*'
        Record -> mempty
{-# INLINE encode #-}

-- | Sends an istatd packet.
send :: MonadIO m => Connection -> Packet -> m ()
send conn packet = sendRaw conn [BSL.toStrict . toLazyByteString $ encode packet]

sendMany :: MonadIO m => Connection -> [Packet] -> m ()
sendMany conn packets = sendRaw conn $
    map (BSL.toStrict . toLazyByteString . mconcat) $
    chunksOf 100 $ map encode packets

sendRaw :: MonadIO m => Connection -> [BS.ByteString] -> m ()
sendRaw (Connection socket addr) packets = liftIO $
    NetBS.sendManyTo socket packets addr
