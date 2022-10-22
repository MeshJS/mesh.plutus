# Simple Auction Contract

### How to use it with Mesh
#### Redeemers
| Haskell | Mesh                                                  |
|---------|-------------------------------------------------------|
| Bid     | `const redeemer: Data = { alternative: 0, fields: [] }` |
| Close   | `const redeemer: Data = { alternative: 1, fields: [] }` |
| End     | `const redeemer: Data = { alternative: 2, fields: [] }` |


#### Datum (untested)
```javascript
const highBid: Data = { alternative: 1, fields: [] } //if there are no bids
const highBid: Data = { alternative: 0, fields: ['<highest bidder address>', gighestBid] } //if there is a bid

const interval: Data = { alternative: 0, fields: [currentPOSIXTime, currentPOSIXTime + intervalLength] }

const token: Data = { alternative: 0, fields: ['<policy Id>', '<token name in hex>'] }

const datum: Data = { alternative: 0, fields: ['<seller address>', minBid, highBid, interval, token] };
```

#### Compiled Script
https://github.com/MartifyLabs/mesh.plutus/blob/auction/auction.plutus
