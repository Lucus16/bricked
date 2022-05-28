module Brick.Widgets.StructuralEditor where

import Data.Text (Text)

data Partial a
  = Empty
  | Editing Text
  | Complete a

data Editor a = Editor
  {
  }
