{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Generate.Data where

import Control.Applicative ((<|>))
import Control.Exception (throw, Exception)
import Data.Char (isAlphaNum, toLower, toUpper)
import Data.Functor.Foldable
import Data.Graph.Inductive (Gr)
import Data.Graph.Inductive qualified as Gr
import Data.Set (Set)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import GHC.Generics (Generic)
import TreeSitter.Grammar

import Debug.Trace

newtype Name = Name { getName :: Text }
  deriving newtype (Eq, Ord, Show, IsString)

-- | Preserves tokens used as keys, and renames them to value
type TokenMap = Map Text (Maybe Text)

isHidden :: RuleName -> Bool
isHidden n = Text.head n == '_'

isInternal :: RuleName -> Bool
isInternal n = let segments = Text.split (== '_') n
  in any (Text.isPrefixOf "internal") segments

shouldInline :: Map RuleName Rule -> RuleName -> Maybe Rule
shouldInline rules name
  | isHidden name = Nothing
      -- case getRule rules name of
      --   ChoiceRule {} -> Nothing
      --   AliasRule {..} -> Just content
      --   r -> Just r
  | otherwise = Nothing
  
data Type
  = Ref Name
  | List Type
  | NonEmpty Type
  | Unit
  | Token Text
  | Tuple Type Type
  | Maybe Type
  | Content
  deriving (Eq, Show)

data Field
  = Named { fName :: Name
          , fType :: Type
          }
  | Unnamed { fType :: Type }
  deriving (Show)

fieldName :: Field -> Maybe Name
fieldName (Named n _) = Just n
fieldName (Unnamed _) = Nothing

data Constructor = Constructor
  { cName :: Text
  , cFields :: [Field]
  } deriving (Show, Generic)

data Node = Node
  { nName :: RuleName
  , nSort :: RuleName
  , nCtrs :: [Constructor]
  } deriving (Show, Generic)

data Syntax = Syntax
  { labels :: Set RuleName
  , nodes :: Set Node
  } deriving (Show, Generic)

newtype GrammarError
  = GrammarErrorUnknownRule RuleName
  deriving (Show)

instance Exception GrammarError

getRule :: Map RuleName Rule -> RuleName -> Rule
getRule rules name = fromMaybe failUnknownRule tryRules
   where
    tryRules = Map.lookup name rules
    failUnknownRule = throw $ GrammarErrorUnknownRule name

topRuleToNode :: TokenMap -> Map RuleName Rule -> RuleName -> Rule -> Node
topRuleToNode tokenMap gr name = \case
  ChoiceRule rules ->
    Node name name
    $ renameConstructors name
    $ Vector.foldr (\rule -> (ruleToConstructor tokenMap gr name rule :)) [] rules
  r@(SeqRule _) -> Node name name [ruleToConstructor tokenMap gr name r]
  AliasRule value _ content ->
    Node name value [Constructor name [ruleToField tokenMap gr content]]
  (ruleToField tokenMap gr -> f) ->
    Node name name [Constructor name [f]]

renameConstructors :: Text -> [Constructor] -> [Constructor]
renameConstructors typeName ctors = go <$> zip ctors [1..]
  where
    go :: (Constructor, Int) -> Constructor
    go (ctor@(Constructor name fields), ord) =
      if name == typeName
         then Constructor (name <> Text.show ord) fields
         else ctor

ruleToConstructor :: TokenMap -> Map RuleName Rule -> RuleName -> Rule -> Constructor
ruleToConstructor tokenMap rules parent = go
  where
    go = \case
      ChoiceRule _ -> error $ "Choice is not a valid constructor: " <> show parent
      BlankRule    -> error $ "Blank is not a valid constructor: " <> show parent
      AliasRule {} -> error $ "Alias is not a valid constructor: " <> show parent
      RepeatRule {} -> error $ "Repeat is not a valid constructor: " <> show parent
      Repeat1Rule {} -> error $ "Repeat1 is not a valid constructor: " <> show parent
      SepByRule {} ->  error $ "SepBy is not a valid constructor: " <> show parent
      SepBy1Rule {} ->  error $ "SepBy1 is not a valid constructor: " <> show parent
      BetweenRule {..} -> Constructor parent [toField content]
      r@(StringRule s) -> Constructor (suffix s) [toField r]
      TokenRule r -> go r
      ImmediateTokenRule r -> go r
      FieldRule {..} -> Constructor (suffix name) [toField content] 
      PrecRule {..} -> go content
      OptionalRule {..} -> go content
      SeqRule {..} -> Constructor parent $ Vector.toList $ toField <$> members
      r@(PatternRule {}) -> Constructor parent [toField r]
      r@(SymbolRule sym) -> Constructor (suffix sym) [toField r]
      r@(RefRule ref) -> Constructor (suffix ref) [toField r]
    toField = ruleToField tokenMap rules
    suffix s = parent <> "_" <> s
  
ruleToField :: TokenMap -> Map RuleName Rule -> Rule -> Field
ruleToField tokenMap rules = para alg
  where
    toType = ruleToType tokenMap rules
    alg :: RuleF (Rule, Field) -> Field
    -- those should be processed upfront
    alg (ChoiceRuleF _) = error "Not hoisted choice rule"
    alg AliasRuleF {} = error "Not hoisted alias rule"
    alg FieldRuleF {..}  = Named (Name nameF) (toType $ fst contentF)
    -- shed a layer
    alg (TokenRuleF r) = snd r
    alg (ImmediateTokenRuleF r) = snd r
    alg (PrecRuleF _ _ r) = snd r
    -- anonymous
    alg r = Unnamed (toType . embed $ fst <$> r)

ruleToType :: TokenMap -> Map RuleName Rule -> Rule -> Type
ruleToType tokenMap rules = go
  where
    go = cata alg
    alg :: RuleF Type -> Type
    alg BlankRuleF = Unit
    -- error "Blank rules should have been elimininated"
    alg ChoiceRuleF {} = error "Choices should have been hoisted to top level"
    alg (AliasRuleF n True _) = Ref (Name n)
    alg (AliasRuleF _ False r) = r

    -- Not used string should have been eliminated
    alg (StringRuleF {..}) = maybe Unit (Token . fromMaybe valueF) (Map.lookup valueF tokenMap)
    alg (PatternRuleF {}) = Content
    alg (SymbolRuleF n@(Name -> s)) =
      case shouldInline rules n of
        Just r -> go r
        Nothing -> Ref s
      -- if isHidden (getName s)
      --    then ruleToType rules (getRule rules n)
      -- -- $ ruleToType (rule $ unName s) -- Node (Name s)
      --    else Ref s
    alg (RefRuleF s@(Name -> n)) =
      case shouldInline rules s of
        Just r -> go r
        Nothing -> Ref n
    alg (SeqRuleF (Vector.uncons -> Nothing)) = error "Empty sequence rule"
    alg (SeqRuleF (Vector.uncons -> Just (h, t))) =
      Vector.foldl mkTuple h t
    alg (SeqRuleF _) = error "Impossible happened"
    alg (RepeatRuleF {..}) = List contentF
    alg (Repeat1RuleF {..}) = NonEmpty contentF
    alg (SepByRuleF {..}) = List contentF
    alg (SepBy1RuleF {..}) = NonEmpty contentF
    alg (OptionalRuleF {..}) = Maybe contentF

    -- The following constructors are transparent:
    alg (BetweenRuleF {..}) = contentF
    alg (TokenRuleF {..}) = contentF
    alg (ImmediateTokenRuleF {..}) = contentF
    alg (FieldRuleF {..}) = contentF
    alg (PrecRuleF {..}) = contentF

    mkTuple :: Type -> Type -> Type
    mkTuple Unit r = r
    mkTuple l Unit = l
    mkTuple l r = Tuple l r

depsOfNode :: Node -> [Name]
depsOfNode (Node _ _ cs) = concatMap depsOfConstructor cs

depsOfConstructor :: Constructor -> [Name]
depsOfConstructor (Constructor _ fs) = concatMap (depsOfType . (.fType)) fs

depsOfType :: Type -> [Name]
depsOfType = go
  where go = \case
          Ref n -> [n]
          List t -> go t
          NonEmpty t -> go t
          Unit -> []
          Token _ -> []
          Tuple a b -> go a <> go b
          Maybe t -> go t
          Content -> []

reachableFrom :: [Node] -> Name -> [Node]
reachableFrom dataTypes start = usedData
 where
  usedData :: [Node]
  usedData =
    [ fromMaybe (error "impossible") (Gr.lab depGraph node)
    | node <- Gr.reachable (nameToNode start) depGraph
    ]

  depGraph :: Gr Node ()
  depGraph = Gr.mkGraph nodesAndData edges

  nodesAndData :: [(Gr.Node, Node)]
  nodesAndData = zip [0 ..] dataTypes

  edges :: [(Gr.Node, Gr.Node, ())]
  edges =
    [ (node, nameToNode depName, ())
    | (node, dataType) <- nodesAndData
    , depName <- depsOfNode dataType
    ]

  nameToNode :: Name -> Gr.Node
  nameToNode name = fromMaybe
    (throw . GrammarErrorUnknownRule . getName $ name)
    (Map.lookup name nameNodeMap)

  nameNodeMap :: Map Name Gr.Node
  nameNodeMap = Map.fromList [ (Name name, node)
                             | (node, Node name _ _) <- nodesAndData
                             ]
