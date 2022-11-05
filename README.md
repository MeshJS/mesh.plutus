# NFT Marketplace Contract

General NFT Marketplace contract. A seller locks an NFT with a certain price and anyone can buy it by sending the demanded price to the seller.

### How to use it with Mesh

In this example we will be selling a token of policy id: `aab618d0373d5239b49695cc613cf1b741e28aadb472f00385138ada` and token name: `MeshToken` which in hex is `4d657368546f6b656e`

#### Put NFT for sale

```javascript
async function sellNFT() {
    const addr = (await wallet.getUsedAddresses())[0];
    const datumConstr: Data = {
      alternative: 0,
      fields: [resolvePaymentKeyHash(addr), 
               10000000, 
               'aab618d0373d5239b49695cc613cf1b741e28aadb472f00385138ada', 
               '4d657368546f6b656e']
    };
    const tx = new Transaction({ initiator: wallet })
      .sendAssets(
        {
          address: scriptAddr,
          datum: {
            value: datumConstr,
          },
        },
        [
          {
            unit: "aab618d0373d5239b49695cc613cf1b741e28aadb472f00385138ada4d657368546f6b656e",
            quantity: "1",
          },
        ],
      );
    const unsignedTx = await tx.build();
    const signedTx = await wallet.signTx(unsignedTx);
    const txHash = await wallet.submitTx(signedTx);
};
```

#### Cancel listing (only available for the seller)
```javascript
async function unlockNFT() {
    const addr = (await wallet.getUsedAddresses())[0];
    const datumConstr: Data = {
      alternative: 0,
      fields: [resolvePaymentKeyHash(addr), 
               10000000, 
               'aab618d0373d5239b49695cc613cf1b741e28aadb472f00385138ada', 
               '4d657368546f6b656e']
    };
    const redeemer: Data = { alternative: 0, fields: [] };
    if (wallet) {
      setLoading(true);
      const assetUtxo = await _getAssetUtxo({
        scriptAddress: scriptAddr, 
        asset: 'aab618d0373d5239b49695cc613cf1b741e28aadb472f00385138ada4d657368546f6b656e',
        datum: datumConstr,
      });
      const tx = new Transaction({ initiator: wallet })
        .redeemValue({
          value: assetUtxo,
          script: script,
          datum: datumConstr,
          redeemer: redeemer,
        })
        .sendValue({ address: addr }, assetUtxo)
        .setRequiredSigners([addr]);
      
      const unsignedTx = await tx.build();
      const signedTx = await wallet.signTx(unsignedTx, true);
      const txHash = await wallet.submitTx(signedTx);
      setLoading(false);
    }
};
```

#### Buy NFT
WIP

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
