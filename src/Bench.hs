{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.DeepSeq
import Data.Bifunctor
import Data.Proxy
import Data.String
import Data.Void
import Data.Word

import Criterion.Main
import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.Attoparsec.ByteString.Lazy as ABL
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Text.Lazy as ATL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Text.Megaparsec as P

newtype Naive s = Naive s
                deriving (Show, Eq, Monoid)

instance NFData1 Naive where
    liftRnf r (Naive a) = r a

instance (NFData a) => NFData (Naive a) where
    rnf = rnf1

wisen :: Proxy (Naive a) -> Proxy a
wisen _ = Proxy

instance (Stream s, Monoid s) => Stream (Naive s) where
    type Token (Naive s) = Token s
    type Tokens (Naive s) = Tokens s
    tokenToChunk = P.tokenToChunk . wisen
    tokensToChunk = P.tokensToChunk . wisen
    chunkToTokens = P.chunkToTokens . wisen
    chunkLength = P.chunkLength . wisen
    chunkEmpty = P.chunkEmpty . wisen
    positionAt1 = P.positionAt1 . wisen
    positionAtN = P.positionAtN . wisen
    advance1 = P.advance1 . wisen
    advanceN = P.advanceN . wisen
    take1_ (Naive s) = second Naive <$> P.take1_ s
    takeN_ n (Naive s) = second Naive <$> P.takeN_ n s
    takeWhile_ f (Naive s) = second Naive (P.takeWhile_ f s)

noFailure :: (IsString s) => Int -> s
noFailure n = fromString $ replicate n 'a'

immediateFailure :: (IsString s) => Int -> s
immediateFailure n = fromString $ 'b' : replicate (n - 1) 'a'

halfFailure :: (IsString s) => Int -> s
halfFailure n = fromString $ let n' = n `div` 2 in replicate n' 'a' ++ replicate n' 'b'

lastFailure :: (IsString s) => Int -> s
lastFailure n = fromString $ replicate (n - 1) 'a' ++ "b"

stringyEnv :: (IsString s) => Int -> IO (s, s, s, s)
stringyEnv n = return (no, imm, half, last)
    where
        !no = noFailure n
        !imm = immediateFailure n
        !half = halfFailure n
        !last = lastFailure n

syntheticCharP :: (Stream s, Token s ~ Char) => Parsec Void s (Tokens s)
syntheticCharP = scanP Nothing '\0' (\st c -> if st /= '\\' && c == 'b' then Nothing else Just c)

syntheticByteP :: (Stream s, Token s ~ Word8) => Parsec Void s (Tokens s)
syntheticByteP = scanP Nothing 0 (\st c -> if st /= 0x5c && c == 0x42 then Nothing else Just c)

syntheticAttoCharP :: AT.Parser T.Text
syntheticAttoCharP = AT.scan '\0' (\st c -> if st /= '\\' && c == 'b' then Nothing else Just c)

syntheticAttoByteP :: AB.Parser BS.ByteString
syntheticAttoByteP = AB.scan '\0' (\st c -> if st /= '\\' && c == 'b' then Nothing else Just c)


strBench (name, size) = env (stringyEnv @String size) $ \(~(no, imm, half, last)) -> bgroup name
                [ bgroup "no failure"
                    [ bench "smart" (nf (runParser syntheticCharP "") no)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive no))
                    ]
                , bgroup "immediate failure"
                    [ bench "smart" (nf (runParser syntheticCharP "") imm)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive imm))
                    ]
                , bgroup "failure half way through"
                    [ bench "smart" (nf (runParser syntheticCharP "") half)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive half))
                    ]
                , bgroup "failure at end"
                    [ bench "smart" (nf (runParser syntheticCharP "") last)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive last))
                    ]
                ]

textBench (name, size) = env (stringyEnv @T.Text size) $ \(~(no, imm, half, last)) -> bgroup name
                [ bgroup "no failure"
                    [ bench "smart" (nf (runParser syntheticCharP "") no)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive no))
                    , bench "attoparsec" (nf (AT.eitherResult . AT.parse syntheticAttoCharP) no)
                    ]
                , bgroup "immediate failure"
                    [ bench "smart" (nf (runParser syntheticCharP "") imm)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive imm))
                    , bench "attoparsec" (nf (AT.eitherResult . AT.parse syntheticAttoCharP) imm)
                    ]
                , bgroup "failure half way through"
                    [ bench "smart" (nf (runParser syntheticCharP "") half)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive half))
                    , bench "attoparsec" (nf (AT.eitherResult . AT.parse syntheticAttoCharP) half)
                    ]
                , bgroup "failure at end"
                    [ bench "smart" (nf (runParser syntheticCharP "") last)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive last))
                    , bench "attoparsec" (nf (AT.eitherResult . AT.parse syntheticAttoCharP) last)
                    ]
                ]

lazyTextBench (name, size) = env (stringyEnv @TL.Text size) $ \(~(no, imm, half, last)) -> bgroup name
                [ bgroup "no failure"
                    [ bench "smart" (nf (runParser syntheticCharP "") no)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive no))
                    , bench "attoparsec" (nf (ATL.eitherResult . ATL.parse syntheticAttoCharP) no)
                    ]
                , bgroup "immediate failure"
                    [ bench "smart" (nf (runParser syntheticCharP "") imm)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive imm))
                    , bench "attoparsec" (nf (ATL.eitherResult . ATL.parse syntheticAttoCharP) imm)
                    ]
                , bgroup "failure half way through"
                    [ bench "smart" (nf (runParser syntheticCharP "") half)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive half))
                    , bench "attoparsec" (nf (ATL.eitherResult . ATL.parse syntheticAttoCharP) half)
                    ]
                , bgroup "failure at end"
                    [ bench "smart" (nf (runParser syntheticCharP "") last)
                    , bench "naive" (nf (runParser syntheticCharP "") (Naive last))
                    , bench "attoparsec" (nf (ATL.eitherResult . ATL.parse syntheticAttoCharP) last)
                    ]
                ]

bsBench (name, size) = env (stringyEnv @BS.ByteString size) $ \(~(no, imm, half, last)) -> bgroup name
                [ bgroup "no failure"
                    [ bench "smart" (nf (runParser syntheticByteP "") no)
                    , bench "naive" (nf (runParser syntheticByteP "") (Naive no))
                    , bench "attoparsec" (nf (AB.eitherResult . AB.parse syntheticAttoByteP) no)
                    ]
                , bgroup "immediate failure"
                    [ bench "smart" (nf (runParser syntheticByteP "") imm)
                    , bench "naive" (nf (runParser syntheticByteP "") (Naive imm))
                    , bench "attoparsec" (nf (AB.eitherResult . AB.parse syntheticAttoByteP) imm)
                    ]
                , bgroup "failure half way through"
                    [ bench "smart" (nf (runParser syntheticByteP "") half)
                    , bench "naive" (nf (runParser syntheticByteP "") (Naive half))
                    , bench "attoparsec" (nf (AB.eitherResult . AB.parse syntheticAttoByteP) half)
                    ]
                , bgroup "failure at end"
                    [ bench "smart" (nf (runParser syntheticByteP "") last)
                    , bench "naive" (nf (runParser syntheticByteP "") (Naive last))
                    , bench "attoparsec" (nf (AB.eitherResult . AB.parse syntheticAttoByteP) last)
                    ]
                ]

lazyBsBench (name, size) = env (stringyEnv @BSL.ByteString size) $ \(~(no, imm, half, last)) -> bgroup name
                [ bgroup "no failure"
                    [ bench "smart" (nf (runParser syntheticByteP "") no)
                    , bench "naive" (nf (runParser syntheticByteP "") (Naive no))
                    , bench "attoparsec" (nf (ABL.eitherResult . ABL.parse syntheticAttoByteP) no)
                    ]
                , bgroup "immediate failure"
                    [ bench "smart" (nf (runParser syntheticByteP "") imm)
                    , bench "naive" (nf (runParser syntheticByteP "") (Naive imm))
                    , bench "attoparsec" (nf (ABL.eitherResult . ABL.parse syntheticAttoByteP) imm)
                    ]
                , bgroup "failure half way through"
                    [ bench "smart" (nf (runParser syntheticByteP "") half)
                    , bench "naive" (nf (runParser syntheticByteP "") (Naive half))
                    , bench "attoparsec" (nf (ABL.eitherResult . ABL.parse syntheticAttoByteP) half)
                    ]
                , bgroup "failure at end"
                    [ bench "smart" (nf (runParser syntheticByteP "") last)
                    , bench "naive" (nf (runParser syntheticByteP "") (Naive last))
                    , bench "attoparsec" (nf (ABL.eitherResult . ABL.parse syntheticAttoByteP) last)
                    ]
                ]

sizes :: [(String, Int)]
sizes = [("small", 500),("medium", 1000),("large", 2000),("xlarge", 4000)]

main :: IO ()
main = defaultMain
    [ bgroup "String" $ strBench <$> sizes
    , bgroup "Data.ByteString" $ bsBench <$> sizes
    , bgroup "Data.ByteString.Lazy" $ lazyBsBench <$> sizes
    , bgroup "Data.Text" $ textBench <$> sizes
    , bgroup "Data.Text.Lazy" $ lazyTextBench <$> sizes
    ]