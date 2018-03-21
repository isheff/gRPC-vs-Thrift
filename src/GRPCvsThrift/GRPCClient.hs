{-# LANGUAGE GADTs             #-}
{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GRPCvsThrift.GRPCClient where

import           GRPCvsThrift.Proto.Bigstructure
import           Network.GRPC.HighLevel.Generated

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad

clientConfig :: ClientConfig
clientConfig = ClientConfig { clientServerHost = "localhost"
                            , clientServerPort = 50051
                            , clientArgs = []
                            , clientSSLConfig = Nothing
                            }


timeout = 10000000 -- maxBound

grpc_client_new :: (MVar BigStructure) -> IO (MVar BigStructure, MVar ())
grpc_client_new server = do 
  {m <- newEmptyMVar 
  ;r <- newEmptyMVar 
  ;forkIO $ withGRPCClient clientConfig $ \client -> do
     {ProtoBigStructure{..} <- protoBigStructureClient client
     ;-- Request for the Ping RPC
     ;ClientNormalResponse _void _meta1 _meta2 _status _details <- protoBigStructurePing (ClientNormalRequest Void timeout [])
     ;forever (do
        {b <- takeMVar m
        ;response <- (protoBigStructureBigStructureCall (ClientNormalRequest b timeout []))
        ;case response of
              (ClientNormalResponse _void _meta1 _meta2 _status _details) -> do
                {b' <- takeMVar server
                ;if b == b'
                   then putMVar r ()
                   else error "GRPC server did not receive the same structure as was transmitted!"}
              (ClientErrorResponse x ) -> error $ "client Error response " ++ (show x)
        })
     }
  ;return (m,r)
  }


grpc_client_big :: (MVar BigStructure, MVar ()) -> BigStructure -> IO ()
grpc_client_big (client_in, client_out) b = putMVar client_in b >> takeMVar client_out
