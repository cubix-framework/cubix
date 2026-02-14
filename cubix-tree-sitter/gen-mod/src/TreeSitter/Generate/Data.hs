{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module TreeSitter.Generate.Data where

import Control.Exception (Exception, throw)
import Data.Map (Map)
import Data.Map qualified as Map (elems, filter, fromList, keys, lookup)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set (empty, fromList, singleton)
import Data.Text (Text)
import Data.Text qualified as Text (head, isPrefixOf, show, split)
import Data.Vector qualified as Vector (foldl, foldr, head, toList, uncons)
import GHC.Generics (Generic)

import Data.Functor.Foldable (Corecursive (..), Recursive (..), cata, para)
import Data.Graph.Inductive (Gr)
import Data.Graph.Inductive qualified as Gr (Node, lab, mkGraph, reachable)

import TreeSitter.Generate.Parser
import TreeSitter.Generate.Types
import TreeSitter.Grammar

isHidden :: RuleName -> Bool
isHidden n = Text.head n == '_'

isInternal :: RuleName -> Bool
isInternal n = let segments = Text.split (== '_') n
  in any (Text.isPrefixOf "internal") segments
  
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
  , cParser :: Parser
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

topRuleToNode :: Map RuleName Rule -> RuleName -> Rule -> Node
topRuleToNode gr name = \case
  ChoiceRule rules ->
    Node name name
    $ renameConstructors name
    $ Vector.foldr (\rule -> (ruleToConstructor gr name rule :)) [] rules
  r@(SeqRule _) -> Node name name [ruleToConstructor gr name r]
  r@(AliasRule value _ content) ->
    Node name value [Constructor name [ruleToField gr content] (mkParser r)]
  r -> Node name name [Constructor name [ruleToField gr r] (mkParser r)]

renameConstructors :: Text -> [Constructor] -> [Constructor]
renameConstructors typeName ctors = go <$> zip ctors [1..]
  where
    go :: (Constructor, Int) -> Constructor
    go (ctor@(Constructor name fields parser), ord) =
      if name == typeName
         then Constructor (name <> Text.show ord) fields parser
         else ctor

ruleToConstructor :: Map RuleName Rule -> RuleName -> Rule -> Constructor
ruleToConstructor rules parent = go
  where
    go = \case
      ChoiceRule _ -> error $ "Choice is not a valid constructor: " <> show parent
      BlankRule    -> error $ "Blank is not a valid constructor: " <> show parent
      AliasRule {} -> error $ "Alias is not a valid constructor: " <> show parent
      RepeatRule {} -> error $ "Repeat is not a valid constructor: " <> show parent
      Repeat1Rule {} -> error $ "Repeat1 is not a valid constructor: " <> show parent
      SepByRule {} ->  error $ "SepBy is not a valid constructor: " <> show parent
      SepBy1Rule {} ->  error $ "SepBy1 is not a valid constructor: " <> show parent
      r@(BetweenRule {..}) -> Constructor parent [toField content] (mkParser r)
      r@(StringRule s) -> Constructor (suffix s) [toField r] (mkParser r)
      TokenRule r -> go r
      ImmediateTokenRule r -> go r
      r@(FieldRule {..}) -> Constructor (suffix name) [toField content] (mkParser r)
      PrecRule {..} -> go content
      OptionalRule {..} -> go content
      r@(SeqRule {..}) -> Constructor parent (Vector.toList $ toField <$> members) (mkParser r)
      r@(PatternRule {}) -> Constructor parent [toField r] (mkParser r)
      r@(SymbolRule sym) -> Constructor (suffix sym) [toField r] (mkParser r)
      r@(RefRule ref) -> Constructor (suffix ref) [toField r] (mkParser r)
    toField = ruleToField rules
    suffix s = parent <> "_" <> s
  
ruleToField :: Map RuleName Rule -> Rule -> Field
ruleToField rules = para alg
  where
    toType = ruleToType rules
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

ruleToType :: Map RuleName Rule -> Rule -> Type
ruleToType _rules = go
  where
    go = cata alg
    alg :: RuleF Type -> Type
    alg BlankRuleF = Unit
    -- error "Blank rules should have been elimininated"
    alg ChoiceRuleF {} = error "Choices should have been hoisted to top level"
    alg (AliasRuleF n True _) = Ref (Name n)
    alg (AliasRuleF _ False r) = r

    -- Some strings are captured in between, sepBy rules. Otherwise
    -- they'll be in our syntax
    alg (StringRuleF {..}) = Token valueF
    alg (PatternRuleF {}) = Content
    alg (SymbolRuleF s) = Ref (Name s)
    alg (RefRuleF n) = Ref (Name n)
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
depsOfConstructor (Constructor _ fs _) = concatMap (depsOfType . (.fType)) fs

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

-- | Collect all tokens (StringRule values) from a grammar rule using catamorphism
tokensOfRule :: Rule -> Set Text
tokensOfRule = cata alg
  where
    alg :: RuleF (Set Text) -> Set Text
    alg = \case
      BlankRuleF -> Set.empty
      StringRuleF {..} -> Set.singleton valueF
      PatternRuleF {} -> Set.empty
      SymbolRuleF {} -> Set.empty
      SeqRuleF {..} -> mconcat (Vector.toList membersF)
      ChoiceRuleF {..} -> mconcat (Vector.toList membersF)
      AliasRuleF {..} -> contentF
      RepeatRuleF {..} -> contentF
      Repeat1RuleF {..} -> contentF
      TokenRuleF {..} -> contentF
      ImmediateTokenRuleF {..} -> contentF
      FieldRuleF {..} -> contentF
      PrecRuleF {..} -> contentF
      RefRuleF {} -> Set.empty
      OptionalRuleF {..} -> contentF
      SepByRuleF {..} -> separatorF <> contentF
      SepBy1RuleF {..} -> separatorF <> contentF
      BetweenRuleF {..} -> Set.fromList [openF, closeF] <> contentF

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
