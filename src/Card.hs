module Card(Card(..)) where

data Card =
    Coffee
  | Half
  | One
  | Two
  | Three
  | Five
  | Eight
  | Thirteen
  | Twenty
  | Forty
  | OneHundred
  | Unknown
  | Infinity

instance Show Card where
  show Coffee     = "☕"
  show Half       = "½"
  show One        = "1"
  show Two        = "2"
  show Three      = "3"
  show Five       = "5"
  show Eight      = "8"
  show Thirteen   = "13"
  show Twenty     = "20"
  show Forty      = "40"
  show OneHundred = "100"
  show Unknown    = "?"
  show Infinity   = "∞"
