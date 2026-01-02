{-# LANGUAGE TemplateHaskell #-}

module BannedConstructorTH (
    mkHasBanned,
    bannedCons
  ) where

import Language.Haskell.TH
import Data.Monoid (Any(..))

import Cubix.Language.SuiMove.Modularized qualified as M

-- | List of banned constructor names
-- Add or remove constructor names here to modify the set of banned constructors
bannedCons :: [Name]
bannedCons =
  [ 'M.Identifier
  , 'M.BinaryExpression1
  , 'M.BinaryExpression2
  , 'M.BinaryExpression3
  , 'M.BinaryExpression4
  , 'M.BinaryExpression5
  , 'M.BinaryExpression6
  , 'M.BinaryExpression7
  , 'M.BinaryExpression8
  , 'M.BinaryExpression9
  -- , 'M.BinaryExpression10  -- Range operator - kept as-is
  , 'M.BinaryExpression11
  , 'M.BinaryExpression12
  , 'M.BinaryExpression13
  , 'M.BinaryExpression14
  , 'M.BinaryExpression15
  , 'M.BinaryExpression16
  , 'M.BinaryExpression17
  , 'M.BinaryExpression18
  , 'M.BinaryExpression19
  , 'M.BinaryExpression20
  , 'M.UnaryExpression
  , 'M.Block
  , 'M.UnitExpression
  , 'M.AssignExpression
  ]

-- | Generate a function that checks if a term contains any banned constructors.
-- Takes a list of constructor Names and generates pattern matches using empty record patterns.
--
-- Example usage:
--   $(mkHasBanned bannedCons)
--
-- Generates code like:
--   hasBanned' (project -> Just (Identifier {})) = pure $ Any True
--   hasBanned' (project -> Just (BinaryExpression4 {})) = pure $ Any True
--   hasBanned' _ = pure $ Any False
mkHasBanned :: [Name] -> Q [Dec]
mkHasBanned constructorNames = do
  -- Create the function name
  let funName = mkName "hasBanned'"

  -- Look up the 'project' function name from Data.Comp.Multi
  projectName <- lookupValueName "project" >>= \case
    Just n -> return n
    Nothing -> fail "Cannot find 'project' function. Make sure Data.Comp.Multi is imported in the calling module."

  -- Generate clauses for each banned constructor
  bannedClauses <- mapM (mkBannedClause projectName) constructorNames

  -- Generate the catch-all clause
  catchAllClause <- do
    body <- [| pure $ Any False |]
    return $ Clause [WildP] (NormalB body) []

  -- Create the function declaration
  let funDecl = FunD funName (bannedClauses ++ [catchAllClause])

  return [funDecl]
  where
    -- Create a clause for a single banned constructor
    mkBannedClause :: Name -> Name -> Q Clause
    mkBannedClause projectName conName = do
      -- Create pattern: (project -> Just (ConName {}))
      let emptyRecPat = RecP conName []  -- Empty record pattern: ConName {}
      let justPat = ConP 'Just [] [emptyRecPat]  -- Just (ConName {})
      let viewPat = ViewP (VarE projectName) justPat  -- project -> Just (ConName {})

      body <- [| pure $ Any True |]

      return $ Clause [viewPat] (NormalB body) []
