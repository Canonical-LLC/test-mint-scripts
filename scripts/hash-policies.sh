cardano-cli address build \
  --payment-script-file scripts/test-policy-0.plutus \
  $BLOCKCHAIN \
  --out-file scripts/$BLOCKCHAIN_PREFIX/test-policy-0.addr

cardano-cli address build \
  --payment-script-file scripts/test-policy-1.plutus \
  $BLOCKCHAIN \
  --out-file scripts/$BLOCKCHAIN_PREFIX/test-policy-1.addr

cardano-cli address build \
  --payment-script-file scripts/test-policy-2.plutus \
  $BLOCKCHAIN \
  --out-file scripts/$BLOCKCHAIN_PREFIX/test-policy-2.addr

cardano-cli address build \
  --payment-script-file scripts/test-policy-3.plutus \
  $BLOCKCHAIN \
  --out-file scripts/$BLOCKCHAIN_PREFIX/test-policy-3.addr
