module HLint.HLint where

import "hint" HLint.Builtin.All
import "hint" HLint.Default
-- import "hint" HLint.Dollar -- TODO: decide if this is a useful rule
import "hint" HLint.Generalise

-- Annoying unless `<>` is in prelude
-- TODO: use custom prelude
ignore "Use mappend"

ignore "Reduce duplication" =
  -- These specs are probably a mess, but ignore for now...
  Data.Blockchain.Core.BlockchainSpec
  Data.Blockchain.Core.Builder.TransactionSpec
