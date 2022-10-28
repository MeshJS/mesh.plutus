# NFT Marketplace Contract

General NFT Marketplace contract. A seller locks an NFT with a certain price and anyone can buy it by sending the demanded price to the seller.

### How to use it with Mesh

#### Redeemers
| Haskell | Mesh                                                  |
|---------|-------------------------------------------------------|
| Buy     | `const redeemer: Data = { alternative: 0, fields: [] }` |
| Close   | `const redeemer: Data = { alternative: 1, fields: [] }` |

#### Datum
```javascript
const datumConstr: Data = {
      alternative: 0,
      fields: ['seller address as pubkeyhash', price, 'policy id of token for sale', 'token name of token for sale in hex']
};
```

#### Compiled Script
https://github.com/MartifyLabs/mesh.plutus/blob/marketplace/marketplace.plutus
