-- https://vaibhavsagar.com/blog/2017/08/13/i-haskell-a-git/
-- https://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( printCommit
    ) where

import qualified Codec.Compression.Zlib           as Z (compress, decompress)
import           Control.Exception                (Exception, SomeException)
import           Control.Monad.Catch              (MonadThrow (..))
import           Data.Attoparsec.ByteString       (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AC
import qualified Data.Attoparsec.Internal.Types
import           Data.Byteable                    (Byteable (toBytes))
import           Data.ByteString                  (ByteString, intercalate)
import qualified Data.ByteString                  as B
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import           Data.ByteString.UTF8             (fromString, toString)
import           Data.Monoid                      (mappend, mconcat, (<>))

type Ref = ByteString

data HGitException = ParserError String | Unknown deriving (Show)
instance Exception HGitException

data Commit = Commit
    { commitTree      :: Ref
    , commitParents   :: [Ref]
    , commitAuthor    :: ByteString
    , commitCommitter :: ByteString
    , commitSignature :: Maybe B.ByteString
    , commitMessage   :: ByteString
    } deriving (Eq, Show)

-- .git/objects/ed/1296f274c0cdb46ae0e0bb4d189b5b14894e70
-- .git/objects/ee/608769a7d6b5d4603d1dd41f6168c77c7051e5
-- .git/objects/14/a3ec5d7888b1606279ed72a4805f24b0e3edbd
printCommit :: IO ()
printCommit = do
    line <- getLine
    commit <- decompress <$> B.readFile line
    parsedCommit <- parse (parseHeader *> parseCommit) commit
    B.putStr $ withHeader "commit" (toBytes parsedCommit)
    c <- parse parseCommit . toBytes $ parsedCommit
    print $ parsedCommit == c
    -- print $ toBytes parsedCommit
    -- either print print (parse (parseHeader *> parseCommit) commit :: Either SomeException Commit)
    -- parsedCommit = parsed (parseHeader *> parseCommit) commit

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

parseSignature :: Parser ByteString
parseSignature = do
    begin <- AC.string " -----BEGIN PGP SIGNATURE-----" <* AC.endOfLine
    sig <- parseRestOfLine `AC.manyTill'` AC.string end
    return . intercalate "\n" $ begin:sig ++ [end]
    where end = " -----END PGP SIGNATURE-----"

parseRestOfLine :: Parser ByteString
parseRestOfLine = AC.takeTill (=='\n') <* AC.endOfLine

parseCommit :: Data.Attoparsec.Internal.Types.Parser ByteString Commit
parseCommit = do
    cTree      <-           AC.string "tree"      *> AC.space *> parseHexRef                   <* AC.endOfLine
    cParents   <- AC.many' (AC.string "parent"    *> AC.space *> parseHexRef                   <* AC.endOfLine)
    cAuthor    <-           AC.string "author"    *> AC.space *> AC.takeTill (AC.inClass "\n") <* AC.endOfLine
    cCommitter <-           AC.string "committer" *> AC.space *> AC.takeTill (AC.inClass "\n") <* AC.endOfLine
    cSignature <- (AC.option Nothing . fmap Just) (AC.string "gpgsig" *> parseSignature        <* AC.endOfLine)
    AC.endOfLine
    Commit cTree cParents cAuthor cCommitter cSignature <$> AC.takeByteString

instance Byteable Commit where
    toBytes (Commit cTree cParents cAuthor cCommitter cSignature cMessage) = mconcat
        [                        "tree "      <> cTree      <> "\n"
        , mconcat (map (\cRef -> "parent "    <> cRef       <> "\n") cParents)
        ,                        "author "    <> cAuthor    <> "\n"
        ,                        "committer " <> cCommitter <> "\n"
        , maybe mempty (\s ->    "gpgsig"     <> s          <> "\n") cSignature
        ,                                                      "\n"
        ,                                        cMessage
        ]

withHeader :: ByteString -> ByteString -> ByteString
withHeader objType content = mconcat [objType, " ", fromString . show $ B.length content, "\NUL", content]