module PureTabs.Sidebar where

import Browser.Runtime as Runtime
import Data.Monoid ((<>))
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, bind, ($), discard)

main :: Effect Unit
main = do 
  log "started sidebar"
  port <- Runtime.connect
  Runtime.onMessageAddListener port onMsg

  where
        onMsg m = do 
           log $ "(sb) tab created: " <> m
