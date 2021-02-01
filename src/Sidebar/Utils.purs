module Sidebar.Utils (whenC) where 

import Halogen (ClassName(..))


whenC :: Boolean -> ClassName -> ClassName
whenC b c = if b then c else ClassName ""
