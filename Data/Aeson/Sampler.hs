{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes #-}
module Data.Aeson.Sampler (fromSample) where

import Data.Aeson
import Data.Monoid
import Data.Scientific
import Data.Text (Text, unpack, pack)
import Data.Text.Lazy (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Control.Monad.Writer
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Ppr
import GHC.Generics (Generic)

fromSample :: QuasiQuoter
fromSample = QuasiQuoter { quoteExp = error "not an exp", quotePat = error "not a pat"
                         , quoteDec = qq, quoteType = error "not a type" }
qq s = let
  (Just js) = decode $ encodeUtf8 $ fromStrict $ pack s
  (t, ds) = runWriter $ format "API" $ to js
  in return (ftch t ++ ds)

ftch t = []
    -- SigD (mkName "fetch") (AppT (ConT ''IO) t)
  -- , FunD (mkName "fetch") [Clause [] (NormalB (ConE (mkName "undefined"))) []]]

format :: Text -> EL -> Writer [Dec] Type
format tx (O m) = do
  let nt = (mkName ("T" ++ (unpack tx)))
  ts <- mapM (\(k, v) -> format k v >>= \x -> return (k, x)) (H.toList m)
  tell $ [
      DataD [] nt [] [(RecC nt (map (\(n, t) -> (mkName (unpack n), NotStrict, t)) ts))] [''Show, ''Generic]
    , InstanceD [] (AppT (ConT ''FromJSON) (ConT nt)) []
    , InstanceD [] (AppT (ConT ''ToJSON) (ConT nt)) []
    ]
  return $ ConT nt
format tx (V e) = do
  t <- format tx e
  return $ AppT (ConT ''V.Vector) t
format _ S = return $ ConT ''Text
format _ N = return $ ConT ''Scientific
format _ B = return $ ConT ''Bool
format _ E = return $ ConT 'Data.Aeson.Null
format _ T = return $ AppT (AppT (ConT ''H.HashMap) (ConT ''Text)) (ConT ''Value)
format _ U = return $ AppT (AppT (ConT ''H.HashMap) (ConT ''Text)) (ConT ''Value)

data EL = O (H.HashMap Text EL)
        | V EL
        -- TODO: maybes: | M EL
        | S | N | B | E
        | T | U
  deriving Show

instance Monoid EL where
  mempty = U
  U `mappend` U = U
  T `mappend` _ = T
  _ `mappend` T = T

  U `mappend` O m = O m
  U `mappend` V x = V x
  U `mappend` S = S
  U `mappend` N = N
  U `mappend` B = B
  U `mappend` E = E

  O m `mappend` U = O m
  V x `mappend` U = V x
  S `mappend` U = S
  N `mappend` U = N
  B `mappend` U = B
  E `mappend` U = E

  O m `mappend` O n = O (H.unionWith mappend m n)
  V x `mappend` V y = V (x `mappend` y)
  S `mappend` S = S
  N `mappend` N = N
  B `mappend` B = B
  E `mappend` E = E

  _ `mappend` _ = T

to :: Value -> EL
to (Object m)   = O (H.map to m)
to (Array v)    = V (mconcat (V.toList (V.map to v)))
to (String _)   = S
to (Number _)   = N
to (Bool _)     = B
to Null         = E
