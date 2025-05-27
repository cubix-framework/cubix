module Cubix.Language.Solidity.ParsePretty where

import Data.Text.IO qualified as Text
import Solidity qualified as Solidity
import Cubix.ParsePretty
import Cubix.Language.Solidity.Parametric.Common
import Cubix.Language.Solidity.Parametric.Full qualified as Full

parseSolidity :: FilePath -> IO (Maybe (MSolidityTerm SolidityL))
parseSolidity path = do
  contents <- Text.readFile path
  let res = Solidity.parseFile path contents
  case res of
    Left  e -> print e >> return Nothing
    Right s -> return $ Just $ translate $ Full.translate s

type instance RootSort MSoliditySig = SolidityL
instance ParseFile MSoliditySig where parseFile = parseSolidity

-- 2023.11.02: Initial Solidity library we're using has no pretty-printer.
