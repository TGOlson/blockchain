# blockchain

Generic blockchain implementation in Haskell. Heavily inspired by Bitcoin blockchain, but does not fully comply to the Bitcoin blockchain spec. Should be suitable for creating arbitrary Bitcoin-like blockchains with in various configurations.

Build

```
$ stack build
```

Test

```
$ stack test                          -- run unit tests
$ ./scripts/test_mining <num-miners>  -- run test mining network
$ ./scripts/test_stats <file-path>    -- print blockchain stats
```

#### notable differences from Bitcoin blockchain

* Merkle root computed with extra leaves at end of tree (compared to extra leaves duplicated in bitcoin)
* Entities serialized as json
* Blockchain config is encoded in blockchain
* Blocks include a dedicated coinbase transaction field to simplify special case handling
* Block headers include an additional field for coinbase transaction hash
* A "transaction in" must declare its previous transaction hash as either a coinbase transaction or a normal transaction

## design goals

* Enforce invariants in types whenever possible (non-empty transactions, genesis block, coinbase transactions, etc.)
* Make it simple to create blockchains with arbitrary configurations
* Make construction of unverified blockchains easy, but provide assurance any validated blockchain instance conforms to expected rules
* Blocks & transactions are never presumed to be a valid part of a blockchain unless present in a validated blockchain
* Adding new blocks to a validated blockchain can assume validity prior parts of the blockchain
* Inspecting unspent transaction outputs of a validated blockchain can assume validity of all transactions
* Blockchain should be readily serializable

## docs

A blockchain is a config and a tree of blocks with each node having a potentially infinite set of branches. A `Blockchain` also includes a tag to note whether is has been verified to meet all the expected conditions -- `Blockchain Validated` or `Blockchain Unvalidated`. Docs are also available on [Hackage](https://hackage.haskell.org/package/blockchain).

```hs
data Blockchain a = Blockchain
    { _config :: BlockchainConfig
    , _node   :: BlockchainNode
    }

data BlockchainNode = BlockchainNode
    { nodeBlock :: Block
    , nodeNodes :: [BlockchainNode]
    }
```

Blockchain construction revolves around three basic functions. Note: the `BlockchainNode` constructor is exported, while the top level `Blockchain` constructor is not.

```hs
-- build an unvalidated blockchain from a config and node
construct :: BlockchainConfig -> BlockchainNode -> Blockchain Unvalidated

-- validate the blockchain
validate :: Blockchain Unvalidated -> Either ValidationException (Blockchain Validated)

-- add a block
addBlock :: Block -> Blockchain Validated -> Either BlockException (Blockchain Validated)
```

Finally, blocks are created by mining. This is a process of finding a `Block` with a certain shape so that it satisfies the expected difficulty, as defined in the blockchain config. Blocks are mined with the coinbase reward going to the provided public key. Note: this function may take a long time to finish executing. Most realistic use cases should run this in a separate thread that can be canceled.

```hs
mineBlock :: PublicKey -> [Transaction] -> Blockchain Validated -> IO (Either MineBlockException Block)
```

## todo

* Test attempts to double spend address funds -- particularly within the same transaction
* Implement `createTransaction` (currently only `createSimpleTransaction`)
* Function that validates transactions -- outside of create transaction logic
* Implement max transaction count per block
* Cache block header hash on block (and maybe transactions) for more efficient operations
  * Maybe cache on the blockchain itself, and `validate` computes/checks hashes?

## references

* http://www.stephendiehl.com/posts/smart_contracts.html
* http://kadena.io/docs/Kadena-PactWhitepaper.pdf
* http://davidederosa.com/basic-blockchain-programming/
* https://en.bitcoin.it/wiki/Transaction
* https://en.bitcoin.it/wiki/Block
* https://en.bitcoin.it/wiki/Block_hashing_algorithm
* https://en.bitcoin.it/wiki/Hashcash
* https://en.bitcoin.it/wiki/Genesis_block
* https://en.bitcoin.it/wiki/Protocol_rules#.22tx.22_messages
