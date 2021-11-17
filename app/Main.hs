import Cardano.Api                         hiding (TxId)
import Prelude
import Canonical.TestMinting
import Data.Foldable

main :: IO ()
main = do
  forM_ [0..3] $ \i -> do
    let testPolicyFilePath = "scripts/test-policy-" <> show i <> ".plutus"
    r <- writeFileTextEnvelope testPolicyFilePath Nothing $ mintingAsCbor i
    case r of
        Left err -> print $ displayError err
        Right () -> putStrLn $ "wrote NFT validator to file " ++ testPolicyFilePath
