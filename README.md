Generic blockchain implementation in Haskell. Close in design to the Bitcoin blockchain, but does not fully comply to the BTC spec.

* http://www.stephendiehl.com/posts/smart_contracts.html
* http://kadena.io/docs/Kadena-PactWhitepaper.pdf
* http://davidederosa.com/basic-blockchain-programming/
* https://en.bitcoin.it/wiki/Transaction
* https://en.bitcoin.it/wiki/Block
* https://en.bitcoin.it/wiki/Block_hashing_algorithm
* https://en.bitcoin.it/wiki/Hashcash
* https://en.bitcoin.it/wiki/Genesis_block


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
