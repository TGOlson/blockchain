# blockchain

Generic blockchain implementation in Haskell. Very, very heavily inspired by Bitcoin blockchain, but does not fully comply to the Bitcoin blockchain spec.

# references

* http://www.stephendiehl.com/posts/smart_contracts.html
* http://kadena.io/docs/Kadena-PactWhitepaper.pdf
* http://davidederosa.com/basic-blockchain-programming/
* https://en.bitcoin.it/wiki/Transaction
* https://en.bitcoin.it/wiki/Block
* https://en.bitcoin.it/wiki/Block_hashing_algorithm
* https://en.bitcoin.it/wiki/Hashcash
* https://en.bitcoin.it/wiki/Genesis_block

# design specs

Goals:
* enforce invariants in types (non-empty transactions, genesis block, coinbase transactions, etc.)
* make construction of arbitrary blockchains easy, but provide assurance any `Blockchain` instance is verified
* `Block`s & `Transaction`s are never presumed to be valid unless part of a `Blockchain`
* adding new blocks to a `Blockchain` can assume validity of existing `Blockchain`
* inspecting unspent transaction outputs of a `Blockchain` can assume validity of all transactions
  * therefore `Blockchain -> HaspMap PubKey Int` is always a valid function and is allowed to error when faced with unexpected conditions
* creating a new blockchain with arbitrary configurations should be simple
* blockchain should be readily serializable

```haskell
-- preserves invariant that a blockchain must have a genesis block
-- encodes config in blockchain
-- no constructors or accessors are exported
data Blockchain = Blockchain BlockchainConfig BlockchainNode
data BlockchainNode = BlockchainNode Block [BlockchainNode]

-- Allows fromjson or ad-hoc blockchain creation,
-- while preserving invariants that all `Blockchain` instances are valid
data UnverifiedBlockchain = UnverifiedBlockchain BlockchainConfig UnverifiedBlockchainNode
data UnverifiedBlockchainNode = UnverifiedBlockchainNode Block [UnverifiedBlockchainNode]
instance FromJSON UnverifiedBlockchain where ...

-- gives users assurance that all `Blockchain` instances are valid
-- that is - correctly solved & ordered blocks, transactions, etc.
-- Note: difficulty is determined by block header
-- block header difficulty must be validated upon block insertion
-- block header difficulty is recalculated every N blocks to desired target solve time
-- using a known formula, so that new blocks always know the target difficulty
verify :: UnverifiedBlockchain -> Either BlockchainVerificationException Blockchain

-- mines a genesis block, and encodes configuration for blockchain
-- including:
--  starting difficulty
--  desired mining rate
--  difficulty recalculation interval
--  initial mining rewards
--  mining reward transitions
createBlockchain :: BlockchainConfig -> Blockchain

-- requires all blocks to have a coinbase transaction
-- this means transaction list can be empty
-- Note: genesis block is essentially a block with no transactions and
-- a made up prevBlockHeaderHash
data Block = Block BlockHeader CoinbaseTransaction [Transaction]

-- insertion
addBlock :: Block -> Blockchain -> Either AddBlockException Blockchain

data CoinbaseTransaction = CoinbaseTransaction (NonEmpty TransactionOut)
data Transaction = Transaction (NonEmpty TransactionIn) (NonEmpty TransactionOut)

data TransactionIn = TransactionIn
    { previousTransactionHash     :: Either (Hash CoinbaseTransaction) (Hash Transaction)
    , previousTransactionOutIndex :: Int
    , signature                   :: Signature
    }

data TransactionOut = TransactionOut
    { value           :: Int
    , signaturePubKey :: PublicKey
    }

-- verification
-- for theoretical "partial-nodes"
validateTransaction :: Blockchain -> Transaction -> Either TransactionException ()

-- transaction creation
createTransaction
    :: [(PrivateKey, PubKey)] -> [(PubKey, Int)] -> Int -> Blockchain
    -> Either CreateTransactionException Transaction
createTransaction srcAddresses targetAddresses fee blockchain = ...

createSimpleTransaction
    :: PrivateKey -> PubKey
    :: PublicKey -> Int -> Int -> Blockchain
    -> Either CreateTransactionException Transaction
createSimpleTransaction srcAddress targetAddress fee blockchain = ...
```

# todo

Now
* get current state of blockchain addresses `Blockchain -> Map String Int`
* clean up testing, many block chain tests don't need quickcheck, should operate on known (valid) chain
* generic assertions against inserts - valid blocks should always insert, invalid never should, etc.
* hide accessors for `Block` data type (only updates can be done via supplied fns)
* find unspent outputs in chain
* function that validates transactions
  * https://en.bitcoin.it/wiki/Protocol_rules#.22tx.22_messages
  * need to consider max transaction count (something akin to blocksize)

Later
* Consider more efficient data format than json
