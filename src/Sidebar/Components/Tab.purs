module PureTabs.Sidebar.Tab  where

import Browser.Tabs (Tab(..))
import Control.Category (identity)
import Data.Const (Const(..))
import Data.Function (const, ($))
import Data.Show (show)
import Data.Unit (Unit)
import Data.Void (Void)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


{-- type State = Tab                                                  --}

{-- component :: forall q i o m. State -> H.Component HH.HTML q i o m --}
{-- component t =                                                     --}
{--   H.mkComponent                                                   --}
{--     { initialState: const t                                       --}
{--     , render: render                                              --}
{--     , eval: H.mkEval H.defaultEval                                --}
{--     }                                                             --}

{-- render :: forall m. State -> H.ComponentHTML Unit () m            --}
{-- render (Tab t) =                                                  --}
{--   HH.div                                                          --}
{--     [ HP.id_ $ show t.id                                          --}
{--     , HP.class_ (H.ClassName "tab")                               --}
{--     ]                                                             --}
{--     [ HH.text t.title ]                                           --}
