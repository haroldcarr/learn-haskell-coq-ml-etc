import           Control.Lens
import           Data.Default
import           Data.Maybe
import           GHCJS.Foreign
import           Reflex
import           Reflex.Dom

import           GHCJS.Foreign
import           GHCJS.Types

main :: IO ()
main = mainWidget $ el "div" $ do
  let req = xhrRequest "GET" "http://localhost:3030/ds/query" def
  pb <- getPostBuild
  asyncReq <- performRequestAsync (tag (constant req) pb)
  resp <- holdDyn Nothing $ fmap (Just . _xhrResponse_status) asyncReq
  text "Response: "
  dynText =<< mapDyn show resp
