module Browser.Tabs (Tab(..)) where

type Tab = {
  active :: Boolean,
  attention :: Boolean,
  audible :: Boolean,
  discarded :: Boolean,
  favIconUrl :: String,
  height :: Number,
  hidden :: Boolean,
  id :: Number,
  incognito :: Boolean,
  index :: Number,
  isArticle :: Boolean,
  pinned :: Boolean,
  status :: String, -- create an enum for that
  successorTabId :: Number,
  title :: String,
  url :: String,
  width :: Number,
  windowId :: Number
}

