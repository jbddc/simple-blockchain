# Simple BlockChain

A simple implementation of a BlockChain in Haskell. Not meant to be used in production.

## TODO list
-   [x] Set limit to transactions per Block
-   [ ]  REST API for clients
-   [x] Persist Block \[and chain\] (using mongoDB)
-   [ ] Build server data cache (storing it in db, associated with block no.)
-   [ ] Docker image for node

### Long shots
-   [ ] API over HTTPS (certificates using LetsEncrypt?)
-   [ ] Basic proof of authority algorithm?
-   [ ] Develop node/client simulator to test blockchain resilience?
