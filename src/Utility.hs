{-# LANGUAGE OverloadedStrings #-}

module Utility
    ( wallet
    , wpkh) where

import Wallet.Emulator.Wallet (Wallet, knownWallet, mockWalletPaymentPubKeyHash, SigningProcess, signPrivateKeys)
import Ledger (PubKeyHash, unPaymentPubKeyHash, CurrencySymbol, TokenName)


wallet :: Integer -> Wallet
wallet = knownWallet

wpkh :: Integer -> PubKeyHash
wpkh = unPaymentPubKeyHash . mockWalletPaymentPubKeyHash . wallet
