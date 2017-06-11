module Data.Blockchain.Core.Builder.Transaction
    ( CreateTransactionException(..)
    , createTransaction
    , createSimpleTransaction
    ) where

import           Control.Monad                   (unless, when)
import           Control.Monad.Trans.Class       (lift)

import qualified Control.Error.Util              as Error
import qualified Control.Monad.Except            as Except
import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString.Lazy            as Lazy
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

createTransaction
    :: [Crypto.KeyPair] -> [(Crypto.PublicKey, Int)] -> Int -> Blockchain.Blockchain
    -> Either CreateTransactionException Blockchain.Transaction
createTransaction _srcs _targets _fee _blockchain = undefined

createSimpleTransaction
    :: Crypto.KeyPair -> Crypto.PublicKey
    -> Word.Word -> Word.Word -> Blockchain.Blockchain
    -> IO (Either CreateTransactionException Blockchain.Transaction)
createSimpleTransaction (Crypto.KeyPair srcPubKey srcPrivKey) targetPubKey value fee blockchain = Except.runExceptT $ do
    let unspentTransactionOutputs = Blockchain.unspentTransactionOutputs blockchain

    txOutPairs <- Error.failWith SourceAddressEmpty $ H.lookup srcPubKey unspentTransactionOutputs

    let txOuts     = snd <$> txOutPairs
        totalValue = sum $ Blockchain.value <$> txOuts

    when (totalValue < (value + fee)) $ Except.throwError SourceAddressInsufficientFunds

    txIns <- sequence $ flip fmap txOutPairs $ \(txOutRef, txOut) -> do
        -- TODO: should keep transaction signing & verification round-tripping in same place
        let txBs = Lazy.toStrict (Aeson.encode txOut)
        sig <- lift $ Crypto.sign srcPrivKey txBs
        unless (Crypto.verify srcPubKey sig txBs) $ Except.throwError InvalidPrivateKey

        return (Blockchain.TransactionIn txOutRef sig)

    let refund          = totalValue - (value + fee)
        txOut           = Blockchain.TransactionOut totalValue targetPubKey
        maybeRefunTxOut = if refund > 0 then Just (Blockchain.TransactionOut refund srcPubKey)
                                        else Nothing
        txOuts'          = maybe (pure txOut) (: pure txOut) maybeRefunTxOut

    return $ Blockchain.Transaction (NonEmpty.fromList txIns) (NonEmpty.fromList txOuts')
