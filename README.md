# Simple BlockChain

Backend for BlockSplitWise application. Based on BlockChain technology.

## Description

Exposes a node that mantains the BlockSplitWise blockchain (communicating with other nodes via Spread). Also exposes a REST server that allows Android clients to interact with node.

## Dependencies

- [Haskell Stack Tool](https://www.haskellstack.org)
- [Spread Toolkit](http://www.spread.org)
- [MongoDB](https://www.mongodb.com)

## Build Project

- `stack setup` (only needed once)
- `stack build`

## Run Project
- `stack exec node local` - run node connecting to localhost spread daemon
- `stack exec node`  - run node connecting to remote spread daemon (WARNING: remote spread daemon may be down...)
