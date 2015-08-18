module AnsiParser.Classes where

import AnsiParser.Types

import qualified Data.Map.Strict as M

addEsc :: Char -> String
addEsc = addEsc' . return -- this works because [] is a monad

addEsc' :: String -> String
addEsc' = ("\x1b" ++)

class ToTerminal a where
  toTerminal :: a -> String

class FromTerminal a where
  fromTerminal :: String -> a

c1Translation :: [(Char, C1)]
c1Translation = [ ('D', Index)
                , ('E', NextLine)
                , ('H', TabSet)
                , ('M', ReverseIndex)
                , ('N', SS2)
                , ('O', SS3)
                  -- , ('P', DeviceControl)
                , ('V', StartGuarded)
                , ('W', EndGuarded)
                , ('Z', ReturnTerminalId)
                , ('^', PrivacyMessage)
                , ('_', APC) ]


instance FromTerminal C1 where
  fromTerminal ('\x1B':c:[]) = parseC1 c
    where parseC1 'D' = Index
          parseC1 'E' = NextLine
          parseC1 'H' = TabSet
          parseC1 'M' = ReverseIndex
          parseC1 'N' = SS2
          parseC1 'O' = SS3
          -- parseC1 'P' = DeviceControl
          parseC1 'V' = StartGuarded
          parseC1 'W' = EndGuarded
          parseC1 'Z' = ReturnTerminalId
          parseC1 '^' = PrivacyMessage
          parseC1 '_' = APC

instance ToTerminal C1 where
  toTerminal = addEsc . c1ToTerminal
     where c1ToTerminal Index = 'D'
           c1ToTerminal NextLine = 'E'
           c1ToTerminal TabSet = 'H'
           c1ToTerminal ReverseIndex = 'M'
           c1ToTerminal SS2 = 'N'
           c1ToTerminal SS3 = 'O'
           -- c1ToTerminal DeviceControl = 'P'
           c1ToTerminal StartGuarded = 'V'
           c1ToTerminal EndGuarded = 'W'
           c1ToTerminal ReturnTerminalId = 'Z'
           c1ToTerminal PrivacyMessage = '^'
           c1ToTerminal APC = '_'

nonPrintTranslation :: [(Char, NonPrint)]
nonPrintTranslation = [ ('\x7', Bell)
                      , ('\x5', Enquiry)
                      , ('\x8', Backspace)
                      , ('\xA', LineFeed)
                      , ('\xB', VerticalTab)
                      , ('\xC', FormFeed)
                      , ('\xD', CarriageReturn)
                      , ('\xE', ShiftIn)
                      , ('\xF', ShiftOut) ]

toNonPrint :: M.Map Char NonPrint
toNonPrint = M.fromList nonPrintTranslation

fromNonPrint :: M.Map NonPrint Char
fromNonPrint = M.fromList $ map (uncurry $ flip (,)) nonPrintTranslation
