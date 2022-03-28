-- Restored from https://github.com/input-output-hk/plutus/pull/4394/files#diff-4b07fe2562138271fb7ab4c93a1024c4ad151a57ea58bf554784eda270682019

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Legacy.Plutus.V1.Ledger.Orphans where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as JSON
import Legacy.Data.Aeson.Extras qualified as JSON
import Data.ByteString qualified as BSS
import PlutusTx.Builtins qualified as PlutusTx


instance ToJSON BSS.ByteString where
    toJSON = JSON.String . JSON.encodeByteString

instance FromJSON BSS.ByteString where
    parseJSON v = JSON.decodeByteString v

instance ToJSON PlutusTx.BuiltinByteString where
    toJSON = JSON.String . JSON.encodeByteString . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
    parseJSON v = PlutusTx.toBuiltin <$> JSON.decodeByteString v

-- Now we restore all the instances that are removed by https://github.com/input-output-hk/plutus/pull/4394

