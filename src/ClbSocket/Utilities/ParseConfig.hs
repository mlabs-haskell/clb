module ClbSocket.Utilities.ParseConfig (getEmulatorConfig) where

import Cardano.Api (Lovelace (Lovelace), lovelaceToValue)
import Clb.MockConfig (defaultBabbage)
import ClbSocket.Core (EmulatorConfig (EmulatorConfig))
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BSL
import Data.Map (Map, elems)

getEmulatorConfig :: IO EmulatorConfig
getEmulatorConfig = do
  rawConfig <- BSL.readFile "./Config/clb-emulator-config.json"
  let vs = case decode @(Map String Integer) rawConfig of
        Just v -> elems $ lovelaceToValue . Lovelace <$> v
        Nothing -> error "Can't decode Emulator Config Values!"
  case vs of
    [totalValue, walletFunds] -> do
      -- Create EmulatorConfig with defaultBabbage
      let emulatorConfig = EmulatorConfig defaultBabbage totalValue walletFunds
      return emulatorConfig
    _ -> error "Can't parse Emulator Config Values!"
