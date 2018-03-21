{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-----------------------------------------------------------------
-- Autogenerated by Thrift Compiler (1.0.0-dev)                      --
--                                                             --
-- DO NOT EDIT UNLESS YOU ARE SURE YOU KNOW WHAT YOU ARE DOING --
-----------------------------------------------------------------

module ThriftBigStructure where
import Prelude (($), (.), (>>=), (==), (++))
import qualified Prelude as P
import qualified Control.Exception as X
import qualified Control.Monad as M ( liftM, ap, when )
import Data.Functor ( (<$>) )
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Hashable as H
import qualified Data.Int as I
import qualified Data.Maybe as M (catMaybes)
import qualified Data.Text.Lazy.Encoding as E ( decodeUtf8, encodeUtf8 )
import qualified Data.Text.Lazy as LT
import qualified GHC.Generics as G (Generic)
import qualified Data.Typeable as TY ( Typeable )
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Data.Vector as Vector
import qualified Test.QuickCheck.Arbitrary as QC ( Arbitrary(..) )
import qualified Test.QuickCheck as QC ( elements )

import qualified Thrift as T
import qualified Thrift.Types as T
import qualified Thrift.Arbitraries as T


import Bigstructure_Types
import qualified ThriftBigStructure_Iface as Iface
-- HELPER FUNCTIONS AND STRUCTURES --

data Ping_args = Ping_args deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Ping_args where
  hashWithSalt salt record = salt  
instance QC.Arbitrary Ping_args where 
  arbitrary = QC.elements [Ping_args]
from_Ping_args :: Ping_args -> T.ThriftVal
from_Ping_args record = T.TStruct $ Map.fromList $ M.catMaybes
  []
write_Ping_args :: T.Protocol p => p -> Ping_args -> P.IO ()
write_Ping_args oprot record = T.writeVal oprot $ from_Ping_args record
encode_Ping_args :: T.StatelessProtocol p => p -> Ping_args -> LBS.ByteString
encode_Ping_args oprot record = T.serializeVal oprot $ from_Ping_args record
to_Ping_args :: T.ThriftVal -> Ping_args
to_Ping_args (T.TStruct fields) = Ping_args{

  }
to_Ping_args _ = P.error "not a struct"
read_Ping_args :: T.Protocol p => p -> P.IO Ping_args
read_Ping_args iprot = to_Ping_args <$> T.readVal iprot (T.T_STRUCT typemap_Ping_args)
decode_Ping_args :: T.StatelessProtocol p => p -> LBS.ByteString -> Ping_args
decode_Ping_args iprot bs = to_Ping_args $ T.deserializeVal iprot (T.T_STRUCT typemap_Ping_args) bs
typemap_Ping_args :: T.TypeMap
typemap_Ping_args = Map.fromList []
default_Ping_args :: Ping_args
default_Ping_args = Ping_args{
}
data Ping_result = Ping_result deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Ping_result where
  hashWithSalt salt record = salt  
instance QC.Arbitrary Ping_result where 
  arbitrary = QC.elements [Ping_result]
from_Ping_result :: Ping_result -> T.ThriftVal
from_Ping_result record = T.TStruct $ Map.fromList $ M.catMaybes
  []
write_Ping_result :: T.Protocol p => p -> Ping_result -> P.IO ()
write_Ping_result oprot record = T.writeVal oprot $ from_Ping_result record
encode_Ping_result :: T.StatelessProtocol p => p -> Ping_result -> LBS.ByteString
encode_Ping_result oprot record = T.serializeVal oprot $ from_Ping_result record
to_Ping_result :: T.ThriftVal -> Ping_result
to_Ping_result (T.TStruct fields) = Ping_result{

  }
to_Ping_result _ = P.error "not a struct"
read_Ping_result :: T.Protocol p => p -> P.IO Ping_result
read_Ping_result iprot = to_Ping_result <$> T.readVal iprot (T.T_STRUCT typemap_Ping_result)
decode_Ping_result :: T.StatelessProtocol p => p -> LBS.ByteString -> Ping_result
decode_Ping_result iprot bs = to_Ping_result $ T.deserializeVal iprot (T.T_STRUCT typemap_Ping_result) bs
typemap_Ping_result :: T.TypeMap
typemap_Ping_result = Map.fromList []
default_Ping_result :: Ping_result
default_Ping_result = Ping_result{
}
data Bigstructurecall_args = Bigstructurecall_args  { bigstructurecall_args_input :: BigStructure
  } deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Bigstructurecall_args where
  hashWithSalt salt record = salt   `H.hashWithSalt` bigstructurecall_args_input record  
instance QC.Arbitrary Bigstructurecall_args where 
  arbitrary = M.liftM Bigstructurecall_args (QC.arbitrary)
  shrink obj | obj == default_Bigstructurecall_args = []
             | P.otherwise = M.catMaybes
    [ if obj == default_Bigstructurecall_args{bigstructurecall_args_input = bigstructurecall_args_input obj} then P.Nothing else P.Just $ default_Bigstructurecall_args{bigstructurecall_args_input = bigstructurecall_args_input obj}
    ]
from_Bigstructurecall_args :: Bigstructurecall_args -> T.ThriftVal
from_Bigstructurecall_args record = T.TStruct $ Map.fromList $ M.catMaybes
  [ (\_v32 -> P.Just (1, ("input",from_BigStructure _v32))) $ bigstructurecall_args_input record
  ]
write_Bigstructurecall_args :: T.Protocol p => p -> Bigstructurecall_args -> P.IO ()
write_Bigstructurecall_args oprot record = T.writeVal oprot $ from_Bigstructurecall_args record
encode_Bigstructurecall_args :: T.StatelessProtocol p => p -> Bigstructurecall_args -> LBS.ByteString
encode_Bigstructurecall_args oprot record = T.serializeVal oprot $ from_Bigstructurecall_args record
to_Bigstructurecall_args :: T.ThriftVal -> Bigstructurecall_args
to_Bigstructurecall_args (T.TStruct fields) = Bigstructurecall_args{
  bigstructurecall_args_input = P.maybe (bigstructurecall_args_input default_Bigstructurecall_args) (\(_,_val34) -> (case _val34 of {T.TStruct _val35 -> (to_BigStructure (T.TStruct _val35)); _ -> P.error "wrong type"})) (Map.lookup (1) fields)
  }
to_Bigstructurecall_args _ = P.error "not a struct"
read_Bigstructurecall_args :: T.Protocol p => p -> P.IO Bigstructurecall_args
read_Bigstructurecall_args iprot = to_Bigstructurecall_args <$> T.readVal iprot (T.T_STRUCT typemap_Bigstructurecall_args)
decode_Bigstructurecall_args :: T.StatelessProtocol p => p -> LBS.ByteString -> Bigstructurecall_args
decode_Bigstructurecall_args iprot bs = to_Bigstructurecall_args $ T.deserializeVal iprot (T.T_STRUCT typemap_Bigstructurecall_args) bs
typemap_Bigstructurecall_args :: T.TypeMap
typemap_Bigstructurecall_args = Map.fromList [(1,("input",(T.T_STRUCT typemap_BigStructure)))]
default_Bigstructurecall_args :: Bigstructurecall_args
default_Bigstructurecall_args = Bigstructurecall_args{
  bigstructurecall_args_input = default_BigStructure}
data Bigstructurecall_result = Bigstructurecall_result deriving (P.Show,P.Eq,G.Generic,TY.Typeable)
instance H.Hashable Bigstructurecall_result where
  hashWithSalt salt record = salt  
instance QC.Arbitrary Bigstructurecall_result where 
  arbitrary = QC.elements [Bigstructurecall_result]
from_Bigstructurecall_result :: Bigstructurecall_result -> T.ThriftVal
from_Bigstructurecall_result record = T.TStruct $ Map.fromList $ M.catMaybes
  []
write_Bigstructurecall_result :: T.Protocol p => p -> Bigstructurecall_result -> P.IO ()
write_Bigstructurecall_result oprot record = T.writeVal oprot $ from_Bigstructurecall_result record
encode_Bigstructurecall_result :: T.StatelessProtocol p => p -> Bigstructurecall_result -> LBS.ByteString
encode_Bigstructurecall_result oprot record = T.serializeVal oprot $ from_Bigstructurecall_result record
to_Bigstructurecall_result :: T.ThriftVal -> Bigstructurecall_result
to_Bigstructurecall_result (T.TStruct fields) = Bigstructurecall_result{

  }
to_Bigstructurecall_result _ = P.error "not a struct"
read_Bigstructurecall_result :: T.Protocol p => p -> P.IO Bigstructurecall_result
read_Bigstructurecall_result iprot = to_Bigstructurecall_result <$> T.readVal iprot (T.T_STRUCT typemap_Bigstructurecall_result)
decode_Bigstructurecall_result :: T.StatelessProtocol p => p -> LBS.ByteString -> Bigstructurecall_result
decode_Bigstructurecall_result iprot bs = to_Bigstructurecall_result $ T.deserializeVal iprot (T.T_STRUCT typemap_Bigstructurecall_result) bs
typemap_Bigstructurecall_result :: T.TypeMap
typemap_Bigstructurecall_result = Map.fromList []
default_Bigstructurecall_result :: Bigstructurecall_result
default_Bigstructurecall_result = Bigstructurecall_result{
}
process_ping (seqid, iprot, oprot, handler) = do
  args <- read_Ping_args iprot
  (X.catch
    (do
      Iface.ping handler
      let res = default_Ping_result
      T.writeMessage oprot ("ping", T.M_REPLY, seqid) $
        write_Ping_result oprot res)
    ((\_ -> do
      T.writeMessage oprot ("ping", T.M_EXCEPTION, seqid) $
        T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")) :: X.SomeException -> P.IO ()))
process_bigstructurecall (seqid, iprot, oprot, handler) = do
  args <- read_Bigstructurecall_args iprot
  (X.catch
    (do
      Iface.bigstructurecall handler (bigstructurecall_args_input args)
      let res = default_Bigstructurecall_result
      T.writeMessage oprot ("bigstructurecall", T.M_REPLY, seqid) $
        write_Bigstructurecall_result oprot res)
    ((\_ -> do
      T.writeMessage oprot ("bigstructurecall", T.M_EXCEPTION, seqid) $
        T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN "")) :: X.SomeException -> P.IO ()))
proc_ handler (iprot,oprot) (name,typ,seqid) = case name of
  "ping" -> process_ping (seqid,iprot,oprot,handler)
  "bigstructurecall" -> process_bigstructurecall (seqid,iprot,oprot,handler)
  _ -> do
    _ <- T.readVal iprot (T.T_STRUCT Map.empty)
    T.writeMessage oprot (name,T.M_EXCEPTION,seqid) $
      T.writeAppExn oprot (T.AppExn T.AE_UNKNOWN_METHOD ("Unknown function " ++ LT.unpack name))
process handler (iprot, oprot) = do
  T.readMessage iprot (
    proc_ handler (iprot,oprot))
  P.return P.True
