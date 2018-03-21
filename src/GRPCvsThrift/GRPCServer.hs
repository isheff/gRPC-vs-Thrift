{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module GRPCvsThrift.GRPCServer where

import GRPCvsThrift.Proto.Bigstructure
import Network.GRPC.HighLevel.Generated

import Control.Concurrent
import Control.Concurrent.MVar
import Data.String (fromString)

handlers :: (MVar BigStructure) -> ProtoBigStructure ServerRequest ServerResponse
handlers m = ProtoBigStructure{protoBigStructurePing = pingHandler
                              ,protoBigStructureBigStructureCall = bigStructureHandler m}
 



pingHandler :: ServerRequest 'Normal Void Void -> IO (ServerResponse 'Normal Void)
pingHandler (ServerNormalRequest _metadata _void) = return (ServerNormalResponse (Void{}) [] StatusOk "")

bigStructureHandler :: (MVar BigStructure) -> ServerRequest 'Normal BigStructure Void -> IO (ServerResponse 'Normal Void)
bigStructureHandler m (ServerNormalRequest _metadata b) = putMVar m b >> return (ServerNormalResponse (Void{}) [] StatusOk "")

options :: ServiceOptions
options = defaultServiceOptions

grpc_server_main :: IO (MVar BigStructure)
grpc_server_main = do
  { m <- newEmptyMVar
  ; forkIO $ protoBigStructureServer (handlers m) options
  ; return m}
