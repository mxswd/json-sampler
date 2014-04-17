{-# LANGUAGE DeriveGeneric #-}
module Riot where

import Data.Aeson.Sampler
-- from https://developer.riotgames.com/api/methods#!/617/1923
[fromSample|
{"champions": [
   {
      "botMmEnabled": true,
      "id": 123,
      "rankedPlayEnabled": false,
      "botEnabled": false,
      "active": true,
      "freeToPlay": true
   },
   {
      "botMmEnabled": false,
      "id": 456,
      "rankedPlayEnabled": true,
      "botEnabled": true,
      "active": false,
      "freeToPlay": false
   }
]}
|]
