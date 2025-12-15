{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}

module TreeSitter.Grammar.Transform.HoistDefinitions (
  hoistDefinitions
) where

import Control.Exception (Exception, throw)
import Control.Monad ((<=<))
import Control.Monad.Reader (MonadReader, Reader, runReader, asks)
import Control.Monad.State (MonadState, StateT, runStateT, modify', gets)
import Data.Bifunctor (second)
import Data.Functor.Foldable hiding (hoist)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Vector qualified as Vector
import TreeSitter.Generate.Data (TokenMap)
import TreeSitter.Grammar

newtype HoistError
  = HoistErrorUnusedString Text
  deriving (Show)

instance Exception HoistError

cataM :: (Monad m, Traversable (Base t), Recursive t)
      => (Base t a -> m a) -- ^ algebra
      -> t -> m a
cataM phi = h
  where h = phi <=< mapM h . project

hoistDefinitions :: TokenMap -> Grammar -> Grammar
hoistDefinitions preserved g@(Grammar {..}) = g { rules = hoistChoiceRules preserved rules }

data HoistState = HoistState
  { fresh :: Int
  , hoisted :: Map RuleName Rule
  }

data HoistEnv = HoistEnv
  { parent :: RuleName
  , definitions :: Map RuleName Rule
  }

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

-- localParent :: (RuleName -> RuleName) -> HoistM a -> HoistM a
-- localParent f = local (\env -> env { parent = f . parent $ env })

-- Turns out there are quite few duplicated rules.  Re-use already
-- hoisted ones.
findRule :: Rule -> HoistM (Maybe RuleName)
findRule rule = do
  rules <- gets hoisted
  pure $ case Map.keys (Map.filter (== rule) rules) of
    k : _ -> Just k
    [] -> Nothing

-- lookupRule :: RuleName -> HoistM (Maybe Rule)
-- lookupRule name = asks (Map.lookup name . definitions)

hoistDedup :: Rule -> HoistM Rule
hoistDedup rule = do
  mexisting <- findRule rule
  RefRule <$> case mexisting of
    Just name -> pure name
    Nothing -> do
      fresh <- getFresh
      parent <- asks parent
      let name = parent `addFieldName` ("internal" <> Text.show fresh)
      modify' (\s -> s { hoisted = Map.insert name rule (hoisted s) })
      pure name

hoist :: RuleName -> Rule -> HoistM Rule
hoist name rule = do
  modify' (\s -> s { hoisted = Map.insert name rule (hoisted s) })
  pure $ RefRule name

extractDefs
  :: TokenMap
  -> Map RuleName Rule   -- ^ Already hoisted rules
  -> RuleName            -- ^ Rule or parent rule name
  -> Rule
  -> ( Rule              -- ^ Updated rule that references new name
     , Map RuleName Rule -- ^ All the choices to hoist
     )
extractDefs preserved rules rulename =
  let initial = HoistState 0 rules
      env = HoistEnv rulename rules
   in runHoist initial env . cataM (extractDefsAlgM preserved)

extractDefsAlgM :: TokenMap -> RuleF Rule -> HoistM Rule
extractDefsAlgM preserved = \case
  ChoiceRuleF {..} -> hoistDedup $ ChoiceRule membersF
  -- rule@(AliasRuleF {..}) -> do
  --   let name = case contentF of
  --         SymbolRule sym -> valueF <> "_" <> sym
  --         -- Unused strings should be removed by now
  --         StringRule string ->
  --           valueF <> "_" <> fromMaybe string
  --             (fromMaybe (throw $ HoistErrorUnusedString string)
  --              $ Map.lookup string preserved)
  --         _ -> valueF
  --   -- make new rule hidden so there is a chance it won't show up in the final AST
  --   hoist (Text.cons '_' name) $ embed rule
  r -> pure $ embed r

hoistChoiceRules :: TokenMap -> Map RuleName Rule -> Map RuleName Rule
hoistChoiceRules preserved rules =
  let (hoisted, updated) = Map.mapAccumWithKey go' rules rules
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
        let (rule', hoisted) = extractDefs preserved acc name rule
        in (acc <> hoisted, rule')
