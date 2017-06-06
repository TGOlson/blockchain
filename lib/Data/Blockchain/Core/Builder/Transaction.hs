module Data.Blockchain.Core.Builder.Transaction
    ( CreateTransactionException(..)
    , createTransaction
    , createSimpleTransaction
    ) where

import qualified Data.Aeson              as Aeson
import qualified Data.HashMap.Strict as H
import qualified Data.Word           as Word

import qualified Data.Blockchain.Core.Crypto     as Crypto
import qualified Data.Blockchain.Core.Blockchain as Blockchain
import qualified Data.Blockchain.Core.Types      as Blockchain

data CreateTransactionException
    = SourceAddressEmpty
    | SourceAddressInsufficientFunds
    | InvalidPrivateKey

createTransaction
    :: [Crypto.KeyPair] -> [(Crypto.PublicKey, Int)] -> Int -> Blockchain.Blockchain
    -> Either CreateTransactionException Blockchain.Transaction
createTransaction _srcs _targets _fee _blockchain = undefined

createSimpleTransaction
    :: Crypto.KeyPair -> Crypto.PublicKey
    -> Word.Word -> Word.Word -> Blockchain.Blockchain
    -> IO (Either CreateTransactionException Blockchain.Transaction)
createSimpleTransaction (Crypto.KeyPair srcPubKey srcPrivKey) targetPubKey value fee blockchain = runEitherT $ do
    let unspentTransactionOutputs = Blockchain.unspentTransactionOutputs blockchain

    txOutPairs <- lift $ case H.lookup srcPubKey unspentTransactionOutputs of
        Nothing -> Left SourceAddressEmpty
        Just v  -> return (Blockchain.TransactionOut v srcPubKey)

    let txOutRefs  = fst <$> txOutPairs
        txOuts     = snd <$> txOutPairs
        totalValue = sum $ value txOuts

    _ <- lift $ if totalValue < (value + fee)
        then Left SourceAddressInsufficientFunds
        else return ()

    -- let txBs = Aeson.encode txout
    let txIns  = (\(txOutRef, txOut -> TransactionIn txOutRef undefined) <$> txOutPairs
        refund = totalValue - (value + fee)
        txOut  = TransactionOut totalValue targetPubKey
        txOutRefund = TransactionOut refund srcPubKey -- Only if there is a refund
        transaction = Transaction txIns [txOut, txOutRefund]

    -- TODO: should keep transaction signing & verification round-tripping in same place
    -- sig <- Crypto.sign srcPrivKey txBs

    -- Verify private key matches pubkey...
    -- _ <- lift $ if Crypto.verify srcPubKey sig txBs
    --     then return ()
    --     else Left InvalidPrivateKey

        -- txIn   = TransactionIn txOutRef sig


    -- data TransactionIn = TransactionIn
    --     { transactionOutRef :: TransactionOutRef
    --     , signature         :: Crypto.Signature -- Signature from prev transaction, using pubkey from prev transaction
    --     }
    --
    -- -- Pointer to a specific TransactionOut
    -- data TransactionOutRef = TransactionOutRef
    --     { transactionHash     :: Either (Crypto.Hash CoinbaseTransaction) (Crypto.Hash Transaction)
    --     , transactionOutIndex :: Word.Word
    --     }
    --
    -- data TransactionOut = TransactionOut
    --     { value           :: Word.Word
    --     , signaturePubKey :: Crypto.PublicKey -- aka. address of where funds go
    --     }
    --
    -- return $ return Transaction
    --     { transactionIn  :: NonEmpty.NonEmpty TransactionIn
    --     , transactionOut :: NonEmpty.NonEmpty TransactionOut
    --     -- TODO: arbitrary bytes?
    --     }
