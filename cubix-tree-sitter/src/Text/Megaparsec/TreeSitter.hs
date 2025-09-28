{-# LANGUAGE RecordWildCards #-}
module Text.Megaparsec.TreeSitter
  ( Lexed (..)
  , Parser
  , pToken
  -- , ParseError
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as ByteString.Char8
import Data.List qualified as List
import Data.List.NonEmpty qualified as NonEmpty
import Data.Proxy (Proxy (Proxy))
import Data.Set qualified as Set
import Data.Void (Void)
import Text.Megaparsec qualified

import Cubix.Language.Info
  ( SourcePos (..)
  , SourceSpan (..)
  , rangeLength
  )
import Cubix.TreeSitter (Token (..))

data Lexed t = Lexed
  { source :: ByteString
  , tokens :: [Token t]
  } deriving Eq

instance Eq t => Ord (Lexed t) where
  compare a b = compare (tokens a) (tokens b)

-- | Parser for tree-sitter generated token streams.
type Parser t = Text.Megaparsec.Parsec Void (Lexed t)

-- type ParseError t = Text.Megaparsec.ParseError (Lexed t) Void

instance Eq t => Text.Megaparsec.Stream (Lexed t) where
  type Token (Lexed t) = Token t
  type Tokens (Lexed t) = [Token t]

  tokenToChunk Proxy x = [x]
  tokensToChunk Proxy xs = xs
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  take1_ (Lexed _ []) = Nothing
  take1_ (Lexed src (t : ts)) =
    Just (t, Lexed src ts)
  takeN_ n (Lexed src s)
    | null s = Nothing
    | otherwise =
      let (x, s') = splitAt n s
       in Just (x, Lexed src s')
  takeWhile_ f (Lexed src s) =
    let (x, s') = List.span f s
     in (x, Lexed src s')

instance (Eq t, Show t) => Text.Megaparsec.VisualStream (Lexed t) where
  showTokens Proxy =
    unwords
      . NonEmpty.toList
      . fmap (show . tokenValue)

  tokensLength Proxy = sum . fmap (rangeLength . tokenRange)

instance (Eq t, Show t) => Text.Megaparsec.TraversableStream (Lexed t) where
  reachOffsetNoLine offset Text.Megaparsec.PosState {..} =
    Text.Megaparsec.PosState
      { pstateInput =
          Lexed
            { source = source pstateInput
            , tokens = post
            }
      , pstateOffset = max pstateOffset offset
      , pstateSourcePos = newSourcePos
      , pstateTabWidth = pstateTabWidth
      , pstateLinePrefix = prefix
      }
   where
    (pre, post) = splitAt (offset - pstateOffset) (tokens pstateInput)
    tokensConsumed = case NonEmpty.nonEmpty pre of
      Nothing -> 0
      Just nePre -> Text.Megaparsec.tokensLength (Proxy @(Lexed t)) nePre
    preBS = ByteString.take tokensConsumed (source pstateInput)
    preStr = ByteString.Char8.unpack preBS
    newSourcePos = case post of
      [] -> pstateSourcePos
      (x : _) -> tokenSourcePos x
    sameLine =
      (==)
        (Text.Megaparsec.sourceLine newSourcePos)
        (Text.Megaparsec.sourceLine pstateSourcePos)
    prefix =
      if sameLine
        then pstateLinePrefix ++ preStr
        else preStr

tokenSourcePos :: Token t -> Text.Megaparsec.SourcePos
tokenSourcePos = spanToSourcePos . tokenSpan

-- tree-sitter uses zero-indexed spans
-- megaparsec expects 1-indexing
spanToSourcePos :: SourceSpan -> Text.Megaparsec.SourcePos
spanToSourcePos (SourceSpan start _end) =
  let SourcePos file row col = start
   in Text.Megaparsec.SourcePos
        file
        (Text.Megaparsec.mkPos $ row + 1)
        (Text.Megaparsec.mkPos $ col + 1)

pToken :: Eq symbol => (symbol -> Maybe a) -> Parser symbol (Token a)
pToken f = do
  Text.Megaparsec.token
    test
    Set.empty
  where
    -- test :: Token symbol -> Maybe (Token a)
    test tok = fmap (\a -> tok {tokenValue = a}) (f (tokenValue tok))
