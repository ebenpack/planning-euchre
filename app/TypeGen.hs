{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

import           App                                 (App)
import           Card                                (Card)
import           Command                             (Command)
import           Control.Monad.Reader.Class          (MonadReader)
import           Data.Proxy                          (Proxy (Proxy))
import           Deck                                (Deck)
import           Language.PureScript.Bridge          (BridgeData,
                                                      Language (Haskell),
                                                      SumType, defaultBridge,
                                                      writePSTypes)
import           Language.PureScript.Bridge.Builder  (BridgePart, buildBridge,
                                                      psTypeParameters, (<|>),
                                                      (^==))
import           Language.PureScript.Bridge.SumType  (mkSumType)
import           Language.PureScript.Bridge.TypeInfo (PSType, TypeInfo (TypeInfo, _typeModule, _typeName, _typePackage, _typeParameters),
                                                      typeName)
import           Room                                (Room)
import           Story                               (Story)
import           User                                (User)


psMap :: MonadReader BridgeData m => m PSType
psMap = TypeInfo "purescript-maps" "Data.Map" "Map" <$> psTypeParameters

mapBridge :: BridgePart
mapBridge = typeName ^== "Map" >> psMap

psIntMap :: MonadReader BridgeData m => m PSType
psIntMap = do
    t <- head <$> psTypeParameters
    pure $ TypeInfo "purescript-prim" "Prim" "Array"
        [ TypeInfo "purescript-tuples" "Data.Tuple" "Tuple"
             [ TypeInfo "purescript-prim" "Prim" "Int" []
             , t
             ]
        ]

intMapBridge :: BridgePart
intMapBridge = typeName ^== "IntMap" >> psIntMap

types :: [SumType 'Haskell]
types =
  [ mkSumType (Proxy :: Proxy App)
  , mkSumType (Proxy :: Proxy Card)
  , mkSumType (Proxy :: Proxy Command)
  , mkSumType (Proxy :: Proxy Room)
  , mkSumType (Proxy :: Proxy User)
  ]

main :: IO ()
main =
  writePSTypes "./client/src/App/Types/" (buildBridge (intMapBridge <|> defaultBridge)) types
