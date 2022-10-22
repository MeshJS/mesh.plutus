# NFT Marketplace Contract

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
      fields: ['seller address', price, 'policy id of token for sale', 'token name of token for sale']
};
```

#### Compiled Script
https://github.com/MartifyLabs/mesh.plutus/blob/marketplace-v1/marketplace.plutus
