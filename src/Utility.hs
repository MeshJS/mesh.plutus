{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( wallet
    , wpkh) where

import Wallet.Emulator.Wallet (Wallet, knownWallet, mockWalletPaymentPubKeyHash)
import Ledger (PubKeyHash, unPaymentPubKeyHash)


wallet :: Integer -> Wallet
wallet = knownWallet

wpkh :: Integer -> PubKeyHash
wpkh = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash . wallet
