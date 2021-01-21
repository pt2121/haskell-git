module Lib
    ( runDecompress
    ) where

import qualified Codec.Compression.Zlib           as Z (compress, decompress)
import           Control.Monad.Catch              (Exception, MonadThrow (..))
import           Data.Attoparsec.ByteString       (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AC
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString                  as B
import           Data.ByteString.Lazy             (fromStrict, toStrict)

type Ref = ByteString

data HGitException = ParserError String | Unknown deriving (Show)
instance Exception HGitException

-- .git/objects/ee/608769a7d6b5d4603d1dd41f6168c77c7051e5
-- .git/objects/14/a3ec5d7888b1606279ed72a4805f24b0e3edbd
runDecompress :: IO ()
runDecompress = do
    line <- getLine
    commit <- decompress <$> B.readFile line
    print =<< parse parseHeader commit

compress :: ByteString -> ByteString
compress   = toStrict . Z.compress   . fromStrict

decompress :: ByteString -> ByteString
decompress = toStrict . Z.decompress . fromStrict

parse :: MonadThrow m => Parser a -> ByteString -> m a
parse parser bs = case AC.parseOnly parser bs of
    Left str -> throwM $ ParserError str
    Right y  -> return y

parseHeader :: Parser (ByteString, Int)
parseHeader = do
    objectType <- AC.takeTill AC.isSpace
    AC.space
    len <- AC.decimal
    AC.char '\NUL'
    return (objectType, len)

parseHexRef :: Parser Ref
parseHexRef = AC.take 40
