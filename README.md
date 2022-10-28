# Always True Contract

Simplest contract out there, always returns `True`. Note that before the validation process, the blockchain will check that, when unlocking funds at a script, you provide the exact same datum as when those funds were locked. So whatever datum you put when locking make sure to use the same when unlocking.

### How to use it with Mesh
#### Redeemer and Datum
Can put anything, as it does not enter in the validation process

#### Compiled Script
https://github.com/MartifyLabs/mesh.plutus/blob/always-true/alwaystrue.plutus
