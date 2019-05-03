<p align="center">
  <img height="100" src="https://github.com/sealchain-project/sealchain/blob/develop/images/SEAL.png">
</p>

# Sealchain

## *Putting Wall Street on the Chain*

#### Created by Wall Street Elites! 

Sealchain focuses on financial asset transaction by using **Blockchain + AI technology**, aiming to build the next generation of financial ecosystem – **Machine Finance**. 

Seal chain is **Cardano 2.0**，a financial Cardano with **multi-coin support** and **smart contract** applications.

<p align="center">
  <img height="300" src="https://github.com/sealchain-project/sealchain/blob/develop/images/Github readme1.jpg">
</p>

#### Technical advantages of Sealchain compared to Cardano:

1.	Extended UTXO model supports native multi-token

     In addition to the platform coin, the SEAL main chain also has built-in stabelcoin. The platform coin is used to pay transaction fees and incentive policies. The stablecoin is used in various financial scenarios. The platform dynamically issues or destroys stablecoins according to market conditions.

2.	Smart contract focused on security

     •	Non-turing complete safety-oriented design with succinct syntax and formal verification support.

     •	Expressive syntax and function definition.

     •	Deployed as public source code, and supports upgraded contracts.

     •	Atomic execution (transaction).

     •	Single-sig and multi-sig public-key authorization.

     •	Designed for direct integration with current industrial databases (SQL or NoSQL).





To learn more about Sealchain, please view our [Website](https://sealchain.io) or our [Whitepaper](https://sealchain.io/whitepaper.html)

## Supported Platforms
Windows 
MacOS
Linux.

## Building Sealchain from Source Code

Please note that in this case you have to install external dependencies
by yourself (see below).

### Install Stack

[Stack](https://docs.haskellstack.org/en/stable/README/) is a cross-platform program
for developing Haskell projects.

Recommended way, for all Unix-systems:

    $ curl -ssl https://get.haskellstack.org/ | sh

On macOS it is possible to install it with `brew`:

    $ brew install haskell-stack

### Setup Environment and Dependencies

To install Haskell compiler of required version run:

    $ stack setup

Then install C-preprocessor for Haskell:

    $ stack install cpphs

Finally install C-library for RocksDB.

On Ubuntu:

    $ sudo apt-get install librocksdb-dev

On macOS:

    $ brew install rocksdb

### Jemalloc Notice

Please make sure that you have [jemalloc](http://jemalloc.net/) package, version `4.5.0`.
If you have newer version of it - you will probably get linker errors during building.

### Building

Build core node:

    $ cd sealchain
    [~/sealchain]$ stack build --fast sealchain-node

## For Contributors
Please see [CONTRIBUTING.md](https://github.com/sealchain-project/sealchain/blob/develop/CONTRIBUTING.md)
for more information.

## License

Please see [LICENSE](https://github.com/sealchain-project/sealchain/blob/master/LICENSE) for
more information.
