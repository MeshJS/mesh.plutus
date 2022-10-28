# Multi-Sig Contract

Parameterized contract for multi-sig unlocking. Requires re-compilation to modify the addresses allowed to sign. The parameters are a list of PubKeyHash and an Integer representing the minimum number of signatories from the list required to unlock funds.

### How to use it with Mesh
#### Redeemer and Datum
Can put anything, as it does not enter in the validation process. 

#### Compiled Script
The compiled script here does not work as the PubKeyHashes passed in params are empty. Need to recompile on per-user basis.
https://github.com/MartifyLabs/mesh.plutus/blob/multi-sig/multisig.plutus
