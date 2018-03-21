{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides an instance Iso Thrift.BigStructure GRPC.BigStructure
-- | This means that you can use Data.Types.Isomorphic.from to freely convert between the two.
module GRPCvsThrift.BigStructureConvert () where

-- | The "BigStructure" datatypes in these are different ways of storing the same thing.
import qualified GRPCvsThrift.Proto.Bigstructure as GRPC
import qualified Bigstructure_Types as Thrift


import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Types.Injective (Injective, to)
import Data.Types.Isomorphic (Iso, from)
import qualified Data.Vector as Vector

-- | convert from gRPC to Thrift. 
-- | There is very little of interest here, save for 2 points:
-- | - The "value_oneof" field in gRPC is optional (it might not be filled). This is not the case in thrift, so if it's empty, there will be an error.
-- | - The binary field is strict in gRPC and lazy in Thrift. 
instance Injective GRPC.BigStructure Thrift.BigStructure where
  to GRPC.BigStructure{GRPC.bigStructureChildren = children
                      ,GRPC.bigStructureValueOneof = Just value} =
     Thrift.default_BigStructure{Thrift.bigStructure_children = Vector.map to children
                                ,Thrift.bigStructure_v =
       case value of 
            (GRPC.BigStructureValueOneofI x) -> Thrift.default_BigStructureValue{Thrift.bigStructureValue_i = Just x}
            (GRPC.BigStructureValueOneofY x) -> Thrift.default_BigStructureValue{Thrift.bigStructureValue_y = Just $ fromStrict x}
            (GRPC.BigStructureValueOneofS x) -> Thrift.default_BigStructureValue{Thrift.bigStructureValue_s = Just x}
            (GRPC.BigStructureValueOneofB x) -> Thrift.default_BigStructureValue{Thrift.bigStructureValue_b = Just x}
                                }
     
-- | convert from Thrift to gRPC. 
-- | There is very little of interest here, except that the binary field is lazy in Thrift and strict in gRPC.
instance Injective Thrift.BigStructure GRPC.BigStructure where
  to Thrift.BigStructure{Thrift.bigStructure_children = children
                        ,Thrift.bigStructure_v = value} =
     GRPC.BigStructure{GRPC.bigStructureChildren = Vector.map to children
                      ,GRPC.bigStructureValueOneof = Just (
        case value of
             (Thrift.BigStructureValue{Thrift.bigStructureValue_i = Just x}) -> GRPC.BigStructureValueOneofI x
             (Thrift.BigStructureValue{Thrift.bigStructureValue_y = Just x}) -> GRPC.BigStructureValueOneofY $ toStrict x
             (Thrift.BigStructureValue{Thrift.bigStructureValue_s = Just x}) -> GRPC.BigStructureValueOneofS x
             (Thrift.BigStructureValue{Thrift.bigStructureValue_b = Just x}) -> GRPC.BigStructureValueOneofB x)
                      }

-- | This is entirely inferred. I'm not sure if I even need this line.
instance Iso Thrift.BigStructure GRPC.BigStructure where
    

-- | This is entirely inferred. I'm not sure if I even need this line.
instance Iso GRPC.BigStructure Thrift.BigStructure where