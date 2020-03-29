module PureTabs.Background where

import Data.List

import Browser.Runtime as Runtime
import Browser.Tabs (Tab)
import Browser.Tabs.OnCreated as OnCreated
import Data.Foldable (for_)
import Data.Monoid ((<>))
import Data.Number.Format (toString)
import Effect (Effect)
import Effect.Console (log)
import Effect.Ref as Ref
import Prelude (Unit, bind, ($), discard)

type Ports = Ref.Ref (List Runtime.Port)

main :: Effect Unit
main = do
  log "started background"
  ports <- Ref.new Nil

  Runtime.onConnectAddListener $ onConnect ports

  listener <- OnCreated.mkListener $ sendCreatedTab ports
  OnCreated.addListener listener

  where
        logTabId :: Tab -> Effect Unit
        logTabId tab = do 
           log $ toString tab.id

        sendCreatedTab :: Ports -> Tab -> Effect Unit
        sendCreatedTab portsRef tab = do
           log $ "(bg) tab created" <> tabId
           ports <- Ref.read portsRef
           for_ ports (\p -> Runtime.postMessage p tabId)

            where
                  tabId = toString tab.id

        onConnect :: Ports -> Runtime.Port -> Effect Unit
        onConnect ref port = do 
           log "new connect"
           Ref.modify_ (\ports -> port : ports) ref
