module Guitar where
import           Notes

data SpecificNote = SpecificNote BassString Fret
type BassString = Int
type Fret = Int


data Bass = Bass NumberOfStrings NumberOfFrets
instance Show Bass where
    show (Bass s f) = show s ++ " String"
type NumberOfStrings = Int
type NumberOfFrets = Int

myBass :: Bass
myBass = Bass 4 21

{-
 - the open strings of a 4 string bass are E1, A1, D2, G2
 -}










