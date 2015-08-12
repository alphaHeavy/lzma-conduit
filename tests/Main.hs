import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Control.Monad.Trans.Resource

import qualified Data.ByteString as B
import qualified Data.Conduit as C
import qualified Data.Conduit.List as Cl
import Data.List
import Data.Word
import Data.Conduit.Lzma

main = defaultMain tests

tests =
  [ testGroup "Compress" compressTests
  , testGroup "Decompress" decompressTests
  , testGroup "Chained" chainedTests
  ]

compressTests =
  [ testProperty "compressAndDiscard" prop_compressAndDiscard
  , testProperty "compressAndCheckLength" prop_compressAndCheckLength
  ]

decompressTests =
  [ testProperty "decompressRandom" prop_decompressRandom
  , testProperty "decompressCorrupt" prop_decompressCorrupt
  ]

chainedTests =
  [ testProperty "chain" prop_chain
  , testProperty "compressThenDecompress" prop_compressThenDecompress
  ]

someString :: Gen B.ByteString
someString = do
  val <- listOf $ elements [0..255::Word8]
  return $ B.pack val

someBigString :: Gen B.ByteString
someBigString = resize (8*1024) someString

prop_compressAndDiscard :: Property
prop_compressAndDiscard = monadicIO . forAllM someBigString $ \ str -> do
  run . runResourceT $ Cl.sourceList [str] C.$$ compress Nothing C.=$= Cl.sinkNull

prop_compressAndCheckLength :: Property
prop_compressAndCheckLength = monadicIO . forAllM someBigString $ \ str -> do
  len <- run . runResourceT $ Cl.sourceList [str] C.$$ compress Nothing C.=$= Cl.fold (\ acc el -> acc + B.length el) 0
  -- random strings don't compress very well
  assert (len > B.length str `div` 2)
  assert (len - 64 < B.length str * 2)

prop_chain :: Property
prop_chain = monadicIO . forAllM someBigString $ \ str -> do
  str' <- run . runResourceT $ Cl.sourceList [str] C.$$ compress Nothing C.=$= decompress Nothing C.=$= Cl.consume
  return $ str == B.concat str'

prop_compressThenDecompress :: Property
prop_compressThenDecompress = monadicIO . forAllM someBigString $ \ str -> do
  blob <- run . runResourceT $ Cl.sourceList [str] C.$$ compress Nothing C.=$= Cl.consume
  let blob' = B.concat blob
  randIdx <- pick $ elements [0..B.length blob'-1]
  let resplit = let (x,y) = B.splitAt randIdx blob' in [x,y]
  str' <- run . runResourceT $ Cl.sourceList resplit C.$$ decompress Nothing C.=$= Cl.consume
  return $ str == B.concat str'

prop_decompressRandom :: Property
prop_decompressRandom = expectFailure . monadicIO . forAllM someBigString $ \ str -> do
  header <- run . runResourceT $ Cl.sourceList [] C.$$ compress Nothing C.=$= Cl.consume
  let blob = header ++ [str]
  run $ runResourceT $ Cl.sourceList blob C.$$ decompress Nothing C.=$= Cl.sinkNull

prop_decompressCorrupt :: Property
prop_decompressCorrupt = expectFailure . monadicIO . forAllM someBigString $ \ str -> do
  header <- run . runResourceT $ Cl.sourceList [] C.$$ compress Nothing C.=$= Cl.consume
  let header' = B.concat header
  randVal <- pick $ elements [0..255::Word8]
  randIdx <- pick $ elements [0..B.length header'-1]
  let (left, right) = B.splitAt randIdx header'
      updated = left `B.append` (randVal `B.cons` B.tail right)
      blob = [updated, str]
  run $ runResourceT $ Cl.sourceList blob C.$$ decompress Nothing C.=$= Cl.sinkNull
