module GRPCvsThrift.Timer where 

import GRPCvsThrift.BigStructureConvert
import GRPCvsThrift.BigStructureGenerate
import GRPCvsThrift.ThriftClient
import GRPCvsThrift.GRPCClient
import GRPCvsThrift.GRPCServer
import GRPCvsThrift.ThriftServer

import Control.Concurrent
import Control.Monad.Random
import Data.Time.Clock
import Data.Types.Isomorphic
import System.Random
  
repetitions = 10
size_of_structure = 200 -- it ends up being a little more than this in kb

main :: IO ()
main = do
  { thrift_server <- thrift_server_main 
  ; grpc_server <- grpc_server_main 
  ; threadDelay 200000
  ; thrift <- thrift_client_new 
  ; grpc <- grpc_client_new grpc_server
  ; let grpc_b = map (fst . (runRand (generate_bigstructure size_of_structure)) . mkStdGen) [1..repetitions]
  ; let thrift_b = map from grpc_b
  ; putStrLn ("show thrift_b has length " ++ (show $ length $ show thrift_b)) -- ensures it's strictly evaluated, and gives me a vague idea of the size
  ; putStrLn ("show grpc_b has length " ++ (show $ length $ show grpc_b)) -- ensures it's strictly evaluated, and gives me a vague idea of the size
  ; time_0 <- getCurrentTime
  ; forM_ thrift_b $ thrift_client_big thrift thrift_server
  ; time_1 <- getCurrentTime
  ; putStrLn ("thrift took " ++ (show $ diffUTCTime time_1 time_0))
  ; time_2 <- getCurrentTime
  ; forM_ grpc_b $ grpc_client_big grpc
  ; time_3 <- getCurrentTime
  ; putStrLn ("grpc took " ++ (show $ diffUTCTime time_3 time_2))
  }
