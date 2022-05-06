{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

import           Prelude (IO(..), show, putStrLn, print)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Data.ByteString.Base16 as B16
import           Data.Aeson           (ToJSON, FromJSON,encode)
import           GHC.Generics         (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude     as P
import           Ledger               hiding (Mint, singleton)
import qualified Ledger.Scripts       as Scripts
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Value         as Value
import           Ledger.Ada           as Ada hiding (divide)
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy  as LBS
import           Cardano.Api hiding (Value, TxOut)
import           Cardano.Api.Shelley hiding (Value, TxOut)
import           Codec.Serialise hiding (encode)
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Data.ByteString as B


contractDetails :: ContractDetails
contractDetails = ContractDetails {
    name = "budz",
    maxSupply = 10000,
    nftPolicy = "d5e6bf0500378d4f0da4e8dde6becec7621cd8cbf5cbb9b87013d4cc",
    oref = TxOutRef "08fc5aa81e5c4b44291ae291f7c8c33eac7345ff5bfe315d26419ae63f62a82b" 1
}

-- the policy script that actually represents the NFT policy
mkTokenPolicy :: TokenData -> Action -> ScriptContext -> Bool
mkTokenPolicy tokenData action ctx = case action of
    Mint -> checkMint
    Burn -> checkBurn
    where
        checkMint :: Bool
        checkMint = check flattenMint count
            where
                check [] _ = True
                check ((cs,tn,am):t) n = ownCurrencySymbol ctx == cs && tn == TokenName (tdName tokenData P.<> integerToByteString n) && am == 1 && check t (n+1)

        checkBurn :: Bool
        checkBurn = all (\(cs,_,am) -> cs == ownCurrencySymbol ctx && am == -1) (flattenValue (txInfoMint txInfo))

        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        flattenMint :: [(CurrencySymbol, TokenName, Integer)]
        flattenMint = flattenValue (txInfoMint txInfo)

        count :: Integer
        count = let [(h, v)] = scriptOutputsAt (tdValidatorHash tokenData) txInfo in
            case findDatum h txInfo of
                Just (Datum d) -> case PlutusTx.fromBuiltinData d of
                        Just c -> case let flatValue = flattenValue v in length flatValue == 2 && valueOf v (tdThreadSymbol tokenData) (TokenName "") == 1 of -- expects script output to contain thread token
                            True ->  c - length flattenMint -- needs to be subtraced, since this c is the value of the next count

        integerToByteString :: Integer -> BuiltinByteString
        integerToByteString n
            | n == 0 = "0"
            | n == 1 = "1"
            | n == 2 = "2"
            | n == 3 = "3"
            | n == 4 = "4"
            | n == 5 = "5"
            | n == 6 = "6"
            | n == 7 = "7"
            | n == 8 = "8"
            | n == 9 = "9"
            | otherwise = integerToByteString (n `P.divide` 10) P.<> integerToByteString (n `P.modulo` 10)
    
-- the policy that mints a single token in order to initialize the state machine of the validator (thread token)
mkThreadPolicy :: TxOutRef -> () -> ScriptContext -> Bool
mkThreadPolicy oref action ctx = hasUtxo && checkMint
  where
    txInfo :: TxInfo
    txInfo = scriptContextTxInfo ctx

    hasUtxo :: Bool
    hasUtxo = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs txInfo

    checkMint :: Bool
    checkMint = case flattenValue (txInfoMint txInfo) of
        [(cs, tn, amt)] -> cs  == ownCurrencySymbol ctx && tn == TokenName "" && amt == 1
        _                -> False

-- the validator script that keeps track of the NFT supply and increments the mint id
mkTokenValidator :: ValidatorData -> Integer -> () -> ScriptContext -> Bool
mkTokenValidator validatorData count _ ctx = checkSigner && checkSupply && checkOutput
    where

        checkSigner :: Bool
        checkSigner = any (== True) [vdNftPolicy validatorData `elem` symbols (txOutValue (txInInfoResolved i)) | i <- txInfoInputs txInfo]

        checkSupply :: Bool
        checkSupply = count < vdMaxSupply validatorData || vdMaxSupply validatorData == -1 --checks for max supply; if there is no limit max supply is -1

        checkOutput :: Bool
        checkOutput = let flatValue = flattenValue outValue in length flatValue == 2 && valueOf outValue (vdThreadSymbol validatorData) (TokenName "") == 1 && nextCount == count + length flattenMint

        flattenMint :: [(CurrencySymbol, TokenName, Integer)]
        flattenMint = flattenValue (txInfoMint txInfo)

        txInfo :: TxInfo
        txInfo = scriptContextTxInfo ctx

        outValue :: Value
        nextCount :: Integer
        (outValue, nextCount) = case getContinuingOutputs ctx of
            [o] -> case txOutDatum o of
                Just h -> case findDatum h txInfo of 
                   Just (Datum d) -> case PlutusTx.fromBuiltinData d of
                        Just c -> (txOutValue o, c)


-- Instantiating

threadPolicy :: Scripts.MintingPolicy
threadPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||]) `PlutusTx.applyCode` PlutusTx.liftCode (oref contractDetails)
    where 
        wrap oref = Scripts.wrapMintingPolicy $ mkThreadPolicy oref
    
tokenPolicy :: Scripts.MintingPolicy
tokenPolicy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||]) `PlutusTx.applyCode` PlutusTx.liftCode tokenData
    where 
        wrap tokenData = Scripts.wrapMintingPolicy $ mkTokenPolicy tokenData

threadSymbol :: CurrencySymbol
threadSymbol = scriptCurrencySymbol $ threadPolicy

tokenSymbol :: CurrencySymbol
tokenSymbol = scriptCurrencySymbol $ tokenPolicy

data TokenValidator
instance Scripts.ValidatorTypes TokenValidator where
    type instance RedeemerType TokenValidator = ()
    type instance DatumType TokenValidator = Integer

tokenValidatorInstance :: Scripts.TypedValidator TokenValidator
tokenValidatorInstance = Scripts.mkTypedValidator @TokenValidator
    ($$(PlutusTx.compile [|| mkTokenValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode validatorData)
    $$(PlutusTx.compile [|| wrap ||])
     where
        wrap = Scripts.wrapValidator @Integer @()

tokenValidatorHash :: Scripts.ValidatorHash
tokenValidatorHash = validatorHash (Scripts.validatorScript tokenValidatorInstance)


tokenData :: TokenData
tokenData = TokenData {tdName = name contractDetails, tdValidatorHash = tokenValidatorHash, tdThreadSymbol = threadSymbol}
validatorData :: ValidatorData
validatorData = ValidatorData {vdNftPolicy = nftPolicy contractDetails, vdMaxSupply = maxSupply contractDetails, vdThreadSymbol = threadSymbol}

-- Types

data ContractDetails = ContractDetails {
    name :: !BuiltinByteString,
    maxSupply :: !Integer,
    nftPolicy :: !CurrencySymbol,
    oref :: !TxOutRef
}

data TokenData = TokenData {tdName :: !BuiltinByteString, tdValidatorHash :: !ValidatorHash, tdThreadSymbol :: !CurrencySymbol}
data ValidatorData = ValidatorData {vdNftPolicy :: !CurrencySymbol, vdMaxSupply :: !Integer, vdThreadSymbol :: !CurrencySymbol}
data Action = Mint | Burn

PlutusTx.makeLift ''ValidatorData
PlutusTx.makeIsDataIndexed ''ValidatorData [('ValidatorData,0)]

PlutusTx.makeLift ''TokenData
PlutusTx.makeIsDataIndexed ''TokenData [('TokenData,0)]

PlutusTx.makeLift ''Action
PlutusTx.makeIsDataIndexed ''Action [('Mint,0),('Burn,1)]

-- Serialization

tokenValidator :: Validator
tokenValidator = Scripts.validatorScript tokenValidatorInstance
tokenValidatorSerialized :: B.ByteString
tokenValidatorSerialized = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unValidatorScript tokenValidator) :: PlutusScript PlutusScriptV1)

tokenPolicySerialized :: B.ByteString
tokenPolicySerialized = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unMintingPolicyScript tokenPolicy) :: PlutusScript PlutusScriptV1)

threadPolicySerialized :: B.ByteString
threadPolicySerialized = B16.encode $ serialiseToCBOR ((PlutusScriptSerialised $ SBS.toShort . LBS.toStrict $ serialise $ Plutus.unMintingPolicyScript threadPolicy) :: PlutusScript PlutusScriptV1)


main :: IO ()
main = do
  putStrLn $ "TokenValidator:\n" ++ show tokenValidatorSerialized
  putStrLn $ "--------------------------------------------------"
  putStrLn $ "TokenPolicy:\n" ++ show tokenPolicySerialized
  putStrLn $ "--------------------------------------------------"
  putStrLn $ "ThreadPolicy:\n" ++ show threadPolicySerialized