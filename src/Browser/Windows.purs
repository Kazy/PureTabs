module Browser.Windows (Window, getCurrent) where 

import Browser.Tabs (Tab, WindowId)
import Control.Promise (toAffE, Promise)
import Effect (Effect)
import Effect.Aff (Aff)


type Window = {
  alwaysOnTop :: Boolean,
  focused :: Boolean,
  -- optional
  height :: Number,
  -- optional
  id :: WindowId,
  incognito :: Boolean,
  -- optional
  left :: Number,
  -- optional
  sessionId :: String,
  -- optional
  {-- state :: Null --}
  -- optional
  tabs :: Array Tab,
  -- optional
  title :: String,
  -- optional
  top :: Number,
  -- optional
  {-- type ::  --}
  -- optional
  width :: Number
}

foreign import getCurrentImpl :: Effect (Promise Window)

getCurrent :: Aff Window
getCurrent = toAffE getCurrentImpl
