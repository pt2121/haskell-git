-- slightly modified from https://vaibhavsagar.com/blog/2017/08/13/i-haskell-a-git/
-- resources
-- https://stefan.saasen.me/articles/git-clone-in-haskell-from-the-bottom-up/
-- https://shop.jcoglan.com/building-git/
-- https://codecrafters.io/challenges/git
-- https://wyag.thb.lt/
-- https://jwiegley.github.io/git-from-the-bottom-up/
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( printCommit
    , printTree
    , printBlob
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
import           Data.ByteString.Base16           (decode, encode)
import qualified Data.ByteString.Char8            as BC
import           Data.ByteString.Lazy             (fromStrict, toStrict)
import           Data.ByteString.UTF8             (fromString, toString)
import           Data.Digest.Pure.SHA             (sha1, showDigest)
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
printCommit :: IO ()
printCommit = do
    line <- getLine
    commit <- decompress <$> B.readFile line
    parsedCommit <- parse (parseHeader *> parseCommit) commit
    B.putStr $ withHeader "commit" (toBytes parsedCommit)
    c <- parse parseCommit . toBytes $ parsedCommit
    print $ parsedCommit == c
    print $ hash (withHeader "commit" (toBytes parsedCommit))
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

hash :: ByteString -> Ref
hash = fromString . showDigest . sha1 . fromStrict

-- for tree, instead of the 40-byte hexadecimal representation of a SHA1 hash,
-- the 20-byte representation is use
parseBinRef :: Parser Ref
parseBinRef = encode <$> AC.take 20

newtype Tree = Tree { treeEntries :: [TreeEntry] } deriving (Eq, Show)

data TreeEntry = TreeEntry
    { treeEntryPerms :: ByteString
    , treeEntryName  :: ByteString
    , treeEntryRef   :: Ref
    } deriving (Eq, Show)

parseTreeEntry :: Parser TreeEntry
parseTreeEntry = do
    perms <- fromString <$> AC.many1' AC.digit
    AC.space
    name  <- AC.takeWhile (/='\NUL')
    AC.char '\NUL'
    TreeEntry perms name <$> parseBinRef

-- .git/objects/2f/f78c568de0058b6706c3b6e29210100f7b7266
parseTree :: Parser Tree
parseTree = Tree <$> AC.many' parseTreeEntry

printTree :: IO ()
printTree = do
    line <- getLine
    tree <- decompress <$> B.readFile line
    parsedTree <- parse (parseHeader *> parseTree) tree
    print parsedTree
    t <- parse parseTree . toBytes $ parsedTree
    print $ t == parsedTree

instance Byteable TreeEntry where
    toBytes (TreeEntry perms name ref) = mconcat [perms, " ", name, "\NUL", fst $ decode ref]

instance Byteable Tree where
    toBytes (Tree entries) = mconcat (map toBytes entries)

newtype Blob = Blob { blobContent :: ByteString } deriving (Eq, Show)

parseBlob :: Parser Blob
parseBlob = Blob <$> AC.takeByteString

-- .git/objects/14/a3ec5d7888b1606279ed72a4805f24b0e3edbd
printBlob :: IO ()
printBlob = do
    line <- getLine
    blob <- decompress <$> B.readFile line
    print $ BC.unlines . take 10 . BC.lines $ blob
    parsedBlob <- parse (parseHeader *> parseBlob) blob
    print parsedBlob
    b <- parse parseBlob . toBytes $ parsedBlob
    print $ b == parsedBlob

instance Byteable Blob where
    toBytes (Blob content) = content