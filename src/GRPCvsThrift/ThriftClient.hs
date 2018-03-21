module GRPCvsThrift.ThriftClient where

import qualified ThriftBigStructure_Client as Client

import Thrift
import Thrift.Protocol.Binary
import Thrift.Transport
import Thrift.Transport.Handle
import Thrift.Server

import Control.Concurrent.MVar
import Network


thrift_client_new = do
  transport  <- hOpen ("localhost", PortNumber 9090)
  let binProto = BinaryProtocol transport
  let client = (binProto, binProto)
  Client.ping client
  return client

thrift_client_big client server b = do
  { Client.bigstructurecall client b
  ; b' <- takeMVar server
  ; if b' == b
       then return ()
       else error "Thrift did not transmit the bigstructure correctly!"
  }