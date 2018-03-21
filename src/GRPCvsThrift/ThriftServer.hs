{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides an instance Iso Thrift.BigStructure GRPC.BigStructure
-- | This means that you can use Data.Types.Isomorphic.from to freely convert between the two.
module GRPCvsThrift.ThriftServer  where

-- | The "BigStructure" datatypes in these are different ways of storing the same thing.
import qualified GRPCvsThrift.Proto.Bigstructure as GRPC
import qualified Bigstructure_Types as Thrift


import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Types.Injective (Injective, to)
import Data.Types.Isomorphic (Iso, from)
import qualified Data.Vector as Vector
import ThriftBigStructure (process)
import ThriftBigStructure_Iface


import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Server

import Control.Concurrent.MVar
import Control.Concurrent

newtype ThriftServerHandler = ThriftServerHandler {unwrap :: MVar Thrift.BigStructure}

newThriftServerHandler = do
  log <- newEmptyMVar
  return $ ThriftServerHandler log

instance ThriftBigStructure_Iface ThriftServerHandler where
  ping _ = return ()
  bigstructurecall = putMVar . unwrap

thrift_server_main :: IO (MVar Thrift.BigStructure)
thrift_server_main =  do
  handler <- newThriftServerHandler
  forkIO $ runBasicServer handler process 9090
  return $ unwrap handler