module AnsiParser.Classes where

import AnsiParser.Types

import qualified Data.Map.Strict as M

addEsc :: Char -> String
addEsc = addEsc' . return -- this works because [] is a monad

addEsc' :: String -> String
addEsc' = ("\x1b" ++)

flipTups :: [(a, b)] -> [(b, a)]
flipTups = map (uncurry $ flip (,))

overLefts :: (a -> c) -> [(a, b)] -> [(c, b)]
overLefts f = map (\(l, r) -> (f l, r))

class ToTerminal a where
  toTerminal :: a -> String

class FromTerminal a where
  fromTerminal :: String -> a

c1Translation :: [(String, C1)]
c1Translation = overLefts addEsc [ ('D', Index)
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

toC1 :: M.Map String C1
toC1 = M.fromList c1Translation

fromC1 :: M.Map C1 String
fromC1 = M.fromList $ flipTups c1Translation

instance FromTerminal C1 where
  fromTerminal = (toC1 M.!)
instance ToTerminal C1 where
  toTerminal = (fromC1 M.!)

nonPrintTranslation :: [(String, NonPrint)]
nonPrintTranslation = overLefts return [ ('\x7', Bell)
                                       , ('\x5', Enquiry)
                                       , ('\x8', Backspace)
                                       , ('\xA', LineFeed)
                                       , ('\xB', VerticalTab)
                                       , ('\xC', FormFeed)
                                       , ('\xD', CarriageReturn)
                                       , ('\xE', ShiftIn)
                                       , ('\xF', ShiftOut) ]

toNonPrint :: M.Map String NonPrint
toNonPrint = M.fromList nonPrintTranslation

fromNonPrint :: M.Map NonPrint String
fromNonPrint = M.fromList $ flipTups nonPrintTranslation

instance FromTerminal NonPrint where
  fromTerminal = (toNonPrint M.!)

instance ToTerminal NonPrint where
  toTerminal = (fromNonPrint M.!)
