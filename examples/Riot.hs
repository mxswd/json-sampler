{-# LANGUAGE DeriveGeneric #-}
module Riot where

import Data.Aeson.Sampler

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
