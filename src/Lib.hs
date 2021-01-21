module Lib
    ( runDecompress
    ) where

import qualified Codec.Compression.Zlib as Z (compress, decompress)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import           Data.ByteString.Lazy   (fromStrict, toStrict)

-- .git/objects/ee/608769a7d6b5d4603d1dd41f6168c77c7051e5
runDecompress :: IO ()
runDecompress = do
    line <- getLine 
    commit <- B.readFile line
    print $ decompress commit

compress :: ByteString -> ByteString
compress   = toStrict . Z.compress   . fromStrict

decompress :: ByteString -> ByteString
decompress = toStrict . Z.decompress . fromStrict
