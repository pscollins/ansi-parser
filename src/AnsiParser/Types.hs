{-# LANGUAGE FlexibleInstances  #-}
module AnsiParser.Types where

import qualified Data.ByteString.Char8 as B
import Data.String (IsString)

data Color
  = Black
  | Red
  | Green
  | Brown
  | Blue
  | Magenta
  | Cyan
  | White
  deriving (Show, Eq)

data ColorPos
  = Foreground
  | Background
  deriving (Show, Eq)

data ColorCmd
  = DefaultColor -- other stuff to, TODO
  | Bold
  | Set (Color, ColorPos)
  deriving (Show, Eq)

-- Here, we will handle "ESC ] 0" (set both icon name + window title)
-- by replacing it with two separate commands.
data OSCmd
  = IconName String
  | WindowTitle String
  | ColorOverride Integer String
  | DynamicTextColor String
  | Font String
  deriving (Show, Eq)

data C1
  = Index -- = D
  | NextLine -- = E
  | TabSet -- = H
  | ReverseIndex -- = M
  | SS2 -- Single shift select of G2 character set; = N
  | SS3 -- like SS2, but for G3; = O
  | StartGuarded -- = V
  | EndGuarded -- = W
  | ReturnTerminalId -- = Z
  | PrivacyMessage -- = ^
  | APC -- Application program command, = _
  deriving (Show, Eq, Enum, Ord)
  --  not implemented: | StartString -- = X

data Cmd
  = CSI Char [String] -- Control Sequence (Initiator)
  | SGR [ColorCmd] -- Select Graphic Rendition i.e. colors, fonts
  | OSC OSCmd -- Operating System Command
  | NonPrint NonPrint -- A nonprinting character
  | C1 C1
  deriving (Show, Eq)

data NonPrint
  = Bell
  | Backspace
  | CarriageReturn
  | Enquiry
  | FormFeed
  | LineFeed
  | ShiftIn
  | ShiftOut
  | VerticalTab
  deriving (Show, Eq, Ord, Enum)

data Expr
  = Plain String
  | Cmd Cmd
  deriving (Show, Eq)
