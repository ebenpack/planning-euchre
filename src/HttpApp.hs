{-# LANGUAGE OverloadedStrings #-}
module HttpApp (httpApp) where

import           Network.Wai.Middleware.Static (addBase, noDots, staticPolicy,
                                                (>->))
import qualified Web.Scotty                    as S

httpApp :: S.ScottyM ()
httpApp = do
  S.get "/" $ S.file "./static/index.html"
  S.middleware $ staticPolicy (noDots >-> addBase "static")
