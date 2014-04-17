module Main where

import Riot
import Data.Aeson

-- Just (TAPI {champions = fromList [Tchampions {botMmEnabled = False, freeToPlay = True, botEnabled = False, active = False, id = 4.0, rankedPlayEnabled = False}]})
main = do
  print $ (decode test :: Maybe TAPI)

test = "{\"champions\":[{\"botMmEnabled\":false,\"id\":4,\"rankedPlayEnabled\":false,\"botEnabled\":false,\"active\":false,\"freeToPlay\":true}]}"
