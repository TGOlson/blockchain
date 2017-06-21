module Data.Blockchain.Core.Builder
    ( CreateTransactionException(..)
    -- , createTransaction
    , createSimpleTransaction
    ) where

import           Control.Monad                   (unless, when)
import           Control.Monad.Trans.Class       (lift)

import qualified Control.Error.Util              as Error
import qualified Control.Monad.Except            as Except
import qualified Data.HashMap.Strict             as H
import qualified Data.List.NonEmpty              as NonEmpty
import qualified Data.Word                       as Word

import qualified Data.Blockchain.Core.Blockchain as Blockchain
import qualified Data.Blockchain.Core.Crypto     as Crypto
import qualified Data.Blockchain.Core.Types      as Blockchain

data CreateTransactionException
    = SourceAddressEmpty
    | SourceAddressInsufficientFunds
    | InvalidPrivateKey
  deriving (Eq, Show)

-- TODO
-- createTransaction
--     :: [Crypto.KeyPair] -> [(Crypto.PublicKey, Int)] -> Int -> Blockchain.Blockchain Blockchain.Validated
--     -> Either CreateTransactionException Blockchain.Transaction
-- createTransaction _srcs _targets _fee _blockchain = undefined

-- Haddock TODO: why simple? what does this mean
-- add docs for what each field in type signature means
createSimpleTransaction
    :: Crypto.KeyPair -> Crypto.PublicKey
    -> Word.Word -> Word.Word -> Blockchain.Blockchain Blockchain.Validated
    -> IO (Either CreateTransactionException Blockchain.Transaction)
createSimpleTransaction (Crypto.KeyPair srcPubKey srcPrivKey) targetPubKey value fee blockchain = Except.runExceptT $ do
    let unspentTransactionOutputs = Blockchain.unspentTransactionOutputs blockchain

    txOutPairs <- Error.failWith SourceAddressEmpty $ H.lookup srcPubKey unspentTransactionOutputs

    let txOuts     = snd <$> txOutPairs
        totalValue = sum $ Blockchain.value <$> txOuts

    when (totalValue < (value + fee)) $ Except.throwError SourceAddressInsufficientFunds

    txIns <- sequence $ flip fmap txOutPairs $ \(txOutRef, txOut) -> do
        sig <- lift $ Blockchain.signTransaction srcPrivKey txOut
        unless (Blockchain.verifyTransactionSignature sig txOut) $ Except.throwError InvalidPrivateKey

        return (Blockchain.TransactionIn txOutRef sig)

    let refund           = totalValue - (value + fee)
        txOut            = Blockchain.TransactionOut value targetPubKey
        maybeRefundTxOut = if refund > 0 then Just (Blockchain.TransactionOut refund srcPubKey)
                                         else Nothing
        txOuts'          = maybe (pure txOut) (: pure txOut) maybeRefundTxOut

    return $ Blockchain.Transaction (NonEmpty.fromList txIns) (NonEmpty.fromList txOuts')
