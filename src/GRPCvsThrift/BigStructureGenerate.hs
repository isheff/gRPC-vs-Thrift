{-# LANGUAGE ScopedTypeVariables #-}

module GRPCvsThrift.BigStructureGenerate (generate_bigstructure) where 

import GRPCvsThrift.BigStructureConvert
import GRPCvsThrift.Proto.Bigstructure
import qualified Bigstructure_Types as Thrift


import Control.Monad (liftM)
import Control.Monad.Random (runRand)
import Control.Monad.Random.Class (MonadRandom, getRandom, getRandoms, getRandomR, getRandomRs)
import Data.ByteString (pack)
import Data.Int (Int64)
import qualified Data.Text.Lazy as Text
import Data.Types.Injective (Injective, to)
import Data.Types.Isomorphic (Iso, from)
import qualified Data.Vector as Vector
import Data.Word (Word8)
import System.Random (Random, random, randomR)

instance Random BigStructureValueOneof where
  random = runRand (do 
    { choice <- getRandomR (1 :: Integer,4)
    ; case choice of
            1 -> do { b <- getRandom
                    ; return $ BigStructureValueOneofB b}
            2 -> do { (integer :: Integer) <- getRandomR (fromIntegral (minBound :: Int64), fromIntegral (maxBound :: Int64))
                    ; return $ BigStructureValueOneofI $ fromIntegral integer}
            3 -> do { bound <- getRandomR (10,10000)
                    ; infinite_string <- getRandoms
                    ; return $ BigStructureValueOneofS $ Text.pack $ take bound infinite_string}
            4 -> do { bound <- getRandomR (10,10000)
                    ; (infinite_ints :: [Integer]) <- getRandomRs (fromIntegral (minBound :: Word8), fromIntegral (maxBound :: Word8))
                    ; return $ BigStructureValueOneofY $ pack $ take bound $ map fromIntegral infinite_ints}
    })
  -- | as for ranged queries, we basically just don't have a notion of range, so we're going to ignore that part.
  randomR _ = random


generate_bigstructure :: (MonadRandom m, Integral n) => n -> m BigStructure
generate_bigstructure n = do
  { value <- getRandom
  ; children <- case n of
                     1 -> return Vector.empty
                     2 -> do { b <- generate_bigstructure 1
                             ; return $ Vector.singleton b}
                     x -> do { r <- getRandomR (1 :: Integer ,fromIntegral (x-1))
                             ; left <- generate_bigstructure r
                             ; right <- generate_bigstructure ((fromIntegral n) - r)
                             ; return $ Vector.fromList [left, right]}
  ; return BigStructure{bigStructureValueOneof = Just value
                       ,bigStructureChildren = children}}
