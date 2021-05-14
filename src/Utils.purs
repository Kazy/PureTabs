module PureTabs.Utils where

import Control.Alt (void)
import Control.Bind (pure)
import Control.Monad (class Monad)
import Data.Unit (Unit, unit)


ifU :: forall a m. Monad m => Boolean -> m a -> m Unit
ifU bool act = if bool then void act else pure unit
