module Canonical.TestMinting
  ( mintingAsCbor
  ) where
import           PlutusTx.Prelude
import           Ledger.Typed.Scripts
import           Ledger
import           PlutusTx
import qualified Data.ByteString.Lazy     as LB
import qualified Data.ByteString.Short    as SBS
import           Cardano.Api.Shelley      (PlutusScript (..), PlutusScriptV1)
import           Codec.Serialise

{-# INLINABLE alwaysSucceed #-}
alwaysSucceed :: Integer -> BuiltinData -> ScriptContext -> Bool
alwaysSucceed x _ _ = x == x

alwaysSucceedsMintingPolicy :: Integer -> MintingPolicy
alwaysSucceedsMintingPolicy num = mkMintingPolicyScript $
  $$(compile [|| \(i :: Integer) -> wrapMintingPolicy $ alwaysSucceed i ||])
    `applyCode` liftCode num

mintingAsCbor :: Integer -> PlutusScript PlutusScriptV1
mintingAsCbor
  = PlutusScriptSerialised
  . SBS.toShort
  . LB.toStrict
  . serialise
  . Validator
  . unMintingPolicyScript
  . alwaysSucceedsMintingPolicy
