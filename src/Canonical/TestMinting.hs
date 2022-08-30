module Canonical.TestMinting
  ( mintingAsCbor
  , alwaysSucceedsMintingPolicy
  ) where
import           PlutusTx.Prelude
import           PlutusTx
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV2)
import           Codec.Serialise
import           Plutus.V2.Ledger.Contexts
import           Plutus.V1.Ledger.Scripts
import qualified Cardano.Api.Shelley as Shelly
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Lazy as BSL

{-# INLINABLE alwaysSucceed #-}
alwaysSucceed :: Integer -> BuiltinData -> ScriptContext -> Bool
alwaysSucceed x _ _ = x == x

mintingPolicyHash :: MintingPolicy -> MintingPolicyHash
mintingPolicyHash
  = MintingPolicyHash
  . getScriptHash
  . scriptHash
  . getValidator
  . Validator
  . getMintingPolicy

wrappedPolicy :: Integer -> BuiltinData -> BuiltinData -> ()
wrappedPolicy i x y = check $ alwaysSucceed i x (unsafeFromBuiltinData y)

alwaysSucceedsMintingPolicy :: Integer -> MintingPolicy
alwaysSucceedsMintingPolicy num = mkMintingPolicyScript $
  $$(compile [|| wrappedPolicy ||])
    `applyCode` liftCode num

scriptHash :: Script -> ScriptHash
scriptHash =
    ScriptHash
    . toBuiltin
    . Shelly.serialiseToRawBytes
    . Shelly.hashScript
    . toCardanoApiScript

toCardanoApiScript :: Script -> Shelly.Script Shelly.PlutusScriptV2
toCardanoApiScript
  = Shelly.PlutusScript Shelly.PlutusScriptV2
  . Shelly.PlutusScriptSerialised
  . BSS.toShort
  . BSL.toStrict
  . serialise

mintingAsCbor :: Integer -> PlutusScript PlutusScriptV2
mintingAsCbor
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  . Validator
  . unMintingPolicyScript
  . alwaysSucceedsMintingPolicy
