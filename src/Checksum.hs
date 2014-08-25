module Checksum where

import Crypto.Hash.SHA1 (hashlazy)
import Data.ByteString.Internal (c2w)
import Text.Printf (printf)

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as Strict

sha256 :: String -> String
sha256 s = (toHex . hashlazy . BL.pack) (map c2w s)
  where
    toHex :: Strict.ByteString -> String
    toHex bytes = Strict.unpack bytes >>= printf "%02x"

