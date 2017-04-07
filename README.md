Generic blockchain implementation in Haskell. Close in design to the Bitcoin blockchain, but does not fully comply to the BTC spec.

* http://www.stephendiehl.com/posts/smart_contracts.html
* http://kadena.io/docs/Kadena-PactWhitepaper.pdf
* http://davidederosa.com/basic-blockchain-programming/
* https://en.bitcoin.it/wiki/Transaction
* https://en.bitcoin.it/wiki/Block
* https://en.bitcoin.it/wiki/Block_hashing_algorithm
* https://en.bitcoin.it/wiki/Hashcash
* https://en.bitcoin.it/wiki/Genesis_block

Design Specs:

Goals:
* enforce invariants in types (non-empty transactions, genesis blocks, coinbase transactions)
* makes construction of arbitrary blockchains easy, but allows users to know any `Blockchain` instance is verified
* Blocks & Transactions are never presumed to be valid unless part of a `Blockchain`
* adding newblocks to a `Blockchain` can assume validity of existing `Blockchain`
* inspecting unspentTransactionOutputs of a `Blockchain` can assume validity of all transactions
  * therefore `Blockchain -> HaspMap PubKey Int` is always a valid function and is allowed to error when faced with unexpected invariants.

```haskell
-- Allows fromjson or ad-hoc blockchain creation,
-- while preserving invariants that all `Blockchain` instances are valid
data UnverifiedBlockchain = UnverifiedBlockchain Block [Blockchain]
instance FromJSON UnverifiedBlockchain where ...

-- preserves invariant that a blockchain must have a genesis block
-- allowing the genesis block to have special properties that would make normal blocks invalid
-- aka. no prev. block hash
-- no constructors or accessors are exported
data Blockchain = Blockchain GenesisBlock [BlockchainNode]
data BlockchainNode = BlockchainNode Block [BlockchainNode]

-- mines a genesis block, and encodes configuration for blockchain
-- including:
--  starting difficulty
--  desired mining rate
--  difficulty recalculation interval
--  initial mining rewards
--  mining reward transitions
createBlockchain :: BlockchainConfig -> Blockchain

-- construction
mineGenesisBlock :: Difficulty -> GenesisBlock
blockchainFromGenesisBlock :: GenesisBlock -> Blockchain

-- gives users assurance that all `Blockchain` instances are valid
-- that is - correctly solved & ordered blocks, transactions, etc.
-- Note: difficulty is determined by block header
-- block header difficulty must be validated upon block insertion
-- block header difficulty is recalculated every N blocks to desired target solve time
-- using a known formula, so that new blocks always know the target difficulty
verify :: UnverifiedBlockchain -> Either BlockchainVerificationException Blockchain

-- requires all blocks to have a coinbase transaction
data Block = Block BlockHeader CoinbaseTransaction [Transaction]

-- insertion
addBlock :: Blockchain -> Either AddBlockException Blockchain

-- Allows GenesisBlock to not have a list of transactions
-- and to have a special header, where prevBlockHeaderHash is not included
data GenesisBlock = GenesisBlock GenesisBlockHeader CoinbaseTransaction

-- Does this work? A transaction needs to have a hash specifying that it is a coinbase transaction
-- with this current model, there is no way for a TransactionIn.PreviousTransactionHash to specify a coinbase transaction
-- could
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

```

TODO:

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
