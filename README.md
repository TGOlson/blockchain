# blockchain

Generic blockchain implementation in Haskell. Very, very heavily inspired by Bitcoin blockchain, but does not fully comply to the Bitcoin blockchain spec.

Notable differences from Bitcoin blockchain:

* merkle root computed with extra leaves at end of tree (compared to extra leaves duplicated in bitcoin)
* entities serialized as json (note: might be super inneficient, but simplifies implementation)
* blockchain config is encoded in blockchain, meaning testing & generating new and unique blockchains is arbitrary
* block includes an additional field for the coinbase transaction
  * this simplifies special-case handling for coinbase transactions
* block header includes an additional field for the coinbase transaction hash
* a "transaction in" must declare its previous transaction hash as either a coinbase transaction or a normal transaction
  * this simplifies special-case handling for coinbase transactions

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
  * therefore `Blockchain -> HashMap PubKey Int` is always a valid function and is allowed to error when faced with unexpected conditions
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
--    * ideally as function of chain length
--    * todo: how to represent as serializable format
createBlockchain :: BlockchainConfig -> Blockchain

-- requires all blocks to have a coinbase transaction
-- this means transaction list can be empty
data Block = Block BlockHeader CoinbaseTransaction [Transaction]

data BlockHeader = BlockHeader
    { version                 :: Int
    , prevBlockHeaderHash     :: Hash BlockHeader
    , coinbaseTransactionHash :: Hash CoinbaseTransaction
    , transactionHashTreeRoot :: HashTreeRoot [Transaction]
    , time                    :: Time.UTCTime
    , difficulty              :: Difficulty
    , nonce                   :: Int
    }

-- mine genesis block based on blockchain config
-- will be an internal function used as part of `createBlockchain`
-- a genesis block will have a prev hash of "00000...00000"
-- Note: genesis block is essentially a block with no transactions and
-- a made up prevBlockHeaderHash
-- Note: also need to provide and/or return priv/pub key pair
-- Note 2: it's possible the output of a genesis block can never be spent
-- because it does not have a valid previous transaction
-- that seems ok...
genesisBlock :: Difficulty -> BlockchainConfig -> Block
genesisBlock = ...
  where
    prevBlockHeaderHash = Hash "0000000000000000000000000000000000000000000000000000000000000000"
    difficulty = initialDifficulty blockchainConfig
    transactionHashTreeRoot = hashTreeRoot []

-- insertion
addBlock :: Block -> Blockchain -> Either AddBlockException Blockchain

-- creation
-- General steps:
--  * find difficulty by inspecting config & last block
--  * create coinbase transaction w/ reward from config
--  * get last block hash by finding last block in longest chain
--  * make block header
--  * inc nonce until desired difficulty is found
--  * add new block to blockchain (potentially using internal function that errors in case of invalid block)
-- Note: list of transactions could possibly be invalid
-- where should we enforce validity of list?
-- a new ValidatedTransactionList type, linked to the blockchain?
-- or `MiningSpecification Blockchain [Transaction]` that has to be constructed using smart constructor?
-- Note 2: this should also return a cancel option, incase a new & higher fee transaction comes through
-- Note 3: also need to provide pubkey for coinbase transaction, and/or ratios to split payout
mineBlock :: [Transaction] -> Blockchain -> IO (Block, Blockchain)

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

-- TODO: future mempool considerations
-- TXIDs of transactions that have been verified as valid but which have not yet appeared in a block.
-- Possible form: newtype MemPool = MemPool (HashMap (Hash Transaction) ())
-- addTransactionToMemPool :: MemPool -> Blockchain -> Transaction -> Either AddTransactionToMemPoolException MemPool
-- possible form for mining:
-- mineBlockWithMemPool :: IO MemPool -> [Transaction] -> Blockchain -> IO (Block, Blockchain)
-- allows mining function to grab from mempool
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


```
newtype Difficulty = Difficulty Int
need to be able to turn `Hash a -> Int`
compare difficulty to int
https://en.bitcoin.it/wiki/Difficulty

difficulty_1_target is easiest difficulty (aka, 1 as hash -> 00000....1)

difficulty -> just an inverse representation where larger is higher
difficulty = difficulty_1_target / current_target

current_target is actually the target hash value as a number

most difficult target would be difficulty_1_target/difficulty_1_target -> 1
easiest = difficulty_1_target/1

lower difficulty is... harder

for this, difficulty_1_target = 2^256 (easiest)
(2 :: Integer) ^ (256 :: Int)
> 115792089237316195423570985008687907853269984665640564039457584007913129639936

https://bitcoin.stackexchange.com/questions/10393/mining-computations-by-the-numbers

```
