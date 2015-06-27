{-# LANGUAGE FlexibleInstances  #-}
module AnsiParser.Types where

import qualified Data.ByteString.Char8 as B
import Data.String (IsString)

-- This lets us put off the decision about how we'll represent our characters
class IsString a => ConsoleString a where
  isEscape :: a -> Bool
  -- printable :: a ->

instance ConsoleString [Char] where
  isEscape = ('\x27' `elem`)

instance ConsoleString B.ByteString where
  isEscape = ('\x27' `B.elem`)

data ConsoleString a => Expr a
  = Plain a

-- data Color
--   = Black
--   | Red
--   | Green
--   | Brown
--   | Blue
--   | Magenta
--   | Cyan
--   | White
--   deriving (Show, Eq)

-- data ColorPos
--   = Foreground
--   | Background
--   deriving (Show, Eq)

-- data ColorCmd
--   = Reset -- other stuff to, TODO
--   | Bold
--   | Set (Color, ColorPos)
--   deriving (Show, Eq)

-- -- Here, we will handle "ESC ] 0" (set both icon name + window title)
-- -- by replacing it with two separate commands.
-- data OSCmd
--   = IconName String
--   | WindowTitle String
--   deriving (Show, Eq)
--   -- Not supported:
--   -- ColorOverride Integer String
--   -- DynamicTextColor String
--   -- Font String

-- data DisplayExpr
--   = Text String
--   | CSI Char [String] -- Control Sequence (Initiator)
--   | SGR [ColorCmd] -- Select Graphic Rendition i.e. colors, fonts
--   | OSC OSCmd -- Operating System Command
--   deriving (Show, Eq)
