module Data.Blockchain.Builder
    ( CreateTransactionException(..)
    , createSimpleTransaction
    ) where

import           Control.Monad             (unless, when)
import           Control.Monad.Trans.Class (lift)

import qualified Control.Error.Util        as Error
import qualified Control.Monad.Except      as Except
import qualified Data.HashMap.Strict       as H
import qualified Data.List.NonEmpty        as NonEmpty
import qualified Data.Word                 as Word

import qualified Data.Blockchain.Crypto    as Crypto
import qualified Data.Blockchain.Types     as Blockchain

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

-- | Create a transaction from a single public key address.
createSimpleTransaction
    :: Crypto.KeyPair   -- ^ KeyPair for source address. PrivateKey will be used to sign transaction.
    -> Crypto.PublicKey -- ^ Target address
    -> Word.Word        -- ^ Transaction value
    -> Word.Word        -- ^ Fee
    -> Blockchain.Blockchain Blockchain.Validated -- ^ Validated blockchain
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
