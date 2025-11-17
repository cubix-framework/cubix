{-# LANGUAGE RecordWildCards #-}

module TreeSitter.GenerateAst.Internal.Transform.HoistChoices (
  hoistChoices
) where

import Data.Bifunctor (second)
import Control.Monad.Reader (MonadReader, Reader, runReader, local, asks)
import Control.Monad.State (MonadState, StateT, runStateT, modify', gets)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import TreeSitter.GenerateAst.Internal.Grammar (Grammar (..), Rule (..), RuleName, FieldName)

hoistChoices :: Grammar -> Grammar
hoistChoices g@(Grammar {..}) = g { rules = hoistChoiceRules rules }

data HoistState = HoistState
  { fresh :: Int
  , hoisted :: Map RuleName Rule
  }

newtype HoistEnv = HoistEnv { parent :: RuleName }

newtype HoistM a = HoistM { unHoist :: StateT HoistState (Reader HoistEnv) a }
  deriving (Functor, Applicative, Monad, MonadReader HoistEnv, MonadState HoistState)

runHoist :: HoistState -> HoistEnv -> HoistM a -> (a, Map RuleName Rule)
runHoist state env = second hoisted
  . flip runReader env
  . flip runStateT state
  . unHoist

getFresh :: HoistM Int
getFresh = do
  f <- gets fresh
  modify' (\s -> s { fresh = succ (fresh s) })
  pure f

addFieldName :: RuleName -> FieldName -> RuleName
addFieldName parent field = parent <> "_" <> field

localParent :: (RuleName -> RuleName) -> HoistM a -> HoistM a
localParent f = local (HoistEnv . f . parent)

-- Turns out there are quite few duplicated rules.  Re-use already
-- hoisted ones.
findRule :: Rule -> HoistM (Maybe RuleName)
findRule rule = do
  rules <- gets hoisted
  pure $ case Map.keys (Map.filter (== rule) rules) of
    k : _ -> Just k
    [] -> Nothing

hoist :: Rule -> HoistM Rule
hoist rule = do
  mexisting <- findRule rule
  SymbolRule <$> case mexisting of
    Just name -> pure name
    Nothing -> do
      fresh <- getFresh
      parent <- asks parent
      let name = parent `addFieldName` ("internal" <> Text.show fresh)
      modify' (\s -> s { hoisted = Map.insert name rule (hoisted s) })
      pure name

-- Single rule might have multiple nested choices that we need to collect
extractChoices
  :: Map RuleName Rule   -- ^ Already hoisted rules
  -> RuleName            -- ^ Rule or parent rule name
  -> Rule
  -> ( Rule              -- ^ Updated rule that references new name
     , Map RuleName Rule -- ^ All the choices to hoist
     )
extractChoices rules rulename rule =
  let initial = HoistState 0 rules
      env = HoistEnv rulename
      (rule', hoisted) = runHoist initial env $ go rule
  in (rule', hoisted)
  where
    go :: Rule -> HoistM Rule
    go = \case
      ChoiceRule {..} -> do
        choices <- Vector.mapM go members
        hoist (ChoiceRule choices)
      SeqRule {..} -> do
        members' <- Vector.mapM go members
        pure $ SeqRule members'
      RepeatRule {..} -> do
        content' <- go content
        pure $ RepeatRule content'
      Repeat1Rule {..} -> do
        content' <- go content
        pure $ Repeat1Rule content'
  
      TokenRule {..} -> do
        content' <- go content
        pure $ TokenRule content'
      ImmediateTokenRule {..} -> do
        content' <- go content
        pure $ ImmediateTokenRule content'
      PrecRule {..} -> do
        content' <- go content
        pure $ PrecRule type_ prec content'

      AliasRule {..} ->
        localParent (\_ -> value) $ do
          content' <- go content
          pure $ AliasRule value named content'
      FieldRule {..} ->
        localParent (`addFieldName` name) $ do
          content' <- go content
          pure $ FieldRule name content'

      -- terminal rules
      r -> pure r
      -- BlankRule        -> _
      -- StringRule  {..} -> _
      -- PatternRule {..} -> _
      -- SymbolRule  {..} -> _

hoistChoiceRules :: Map RuleName Rule -> Map RuleName Rule
hoistChoiceRules rules =
  let (hoisted, updated) = Map.mapAccumWithKey go' Map.empty rules
  in updated <> hoisted
  where
    -- ignore top level choices
    go' :: Map RuleName Rule -> RuleName -> Rule -> (Map RuleName Rule, Rule)
    go' acc name rule = case rule of
      ChoiceRule members ->
        let (hoisted, ms) = Vector.foldr'
              (\member (acc', m) ->
                let (acc'', m') = go acc' name member
                in (acc' <> acc'', Vector.cons m' m))
              (acc, Vector.empty)
              members
        in (hoisted, ChoiceRule ms)
      _ -> go acc name rule
    go :: Map RuleName Rule -> RuleName -> Rule -> (Map RuleName Rule, Rule)
    go acc name rule =
        let (rule', hoisted) = extractChoices acc name rule
        in (acc <> hoisted, rule')
