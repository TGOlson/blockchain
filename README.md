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

# todo

Now
* add tests w/ transactions too `addBlock` test
* generate test chain w/ transactions
* implement `createTransaction`
* function that validates transactions
  * https://en.bitcoin.it/wiki/Protocol_rules#.22tx.22_messages
  * need to consider max transaction count (something akin to blocksize)

Later
* Consider more efficient data format than json
