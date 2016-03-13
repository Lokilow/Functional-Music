----------------------------------------------------------------------
module Notes where
import           Control.Applicative
import qualified Data.Map.Strict     as Map


data Note = Note
    {pitch   :: PitchClass
    , octave :: Octave}
    deriving Eq

data Interval = Interval Name Semitones

data PitchClass = C | Db | D | Eb | E | F | Gb | G | Ab | A | Bb | B
    deriving (Show, Eq, Enum, Bounded, Ord)

type Octave = Int
type Semitones = Int
type Name = String


instance Show Note where
    show (Note a b) = (show a) ++ (show b)

instance Show Interval where
    show (Interval a b) = a

instance Ord Note where
    compare (Note p1 o1) (Note p2 o2)
        |o1 < o2 = LT
        |o1 == o2 = (compare p1 p2)
        |o1 > o2 = GT

--Operations
transpose :: Note -> Semitones -> Note
transpose (Note p o) s = (Note (chromatics !! (mod x 12)) (div x 12))
    where x = (length $ takeWhile (/= p) chromatics) + (12 *  o) + s

invert :: Note -> Note -> Note
invert note1 note2 = transpose note2 (note2 `difference` note1)

--Helper Functions
pitchToInt :: PitchClass -> Int
pitchToInt p = case p of
    C -> 0
    Db -> 1
    D -> 2
    Eb -> 3
    E -> 4
    F -> 5
    Gb -> 6
    G -> 7
    Ab -> 8
    A -> 9
    Bb -> 10
    B -> 11

difference :: Note -> Note -> Semitones
difference note1 note2 = pitchDiff + octDiff
    where pitchDiff = (pitchToInt . pitch $ note1) - (pitchToInt . pitch $ note2)
          octDiff = 12 * ((octave note1) - (octave note2))

--Constants
chromatics :: [PitchClass]
chromatics = [C .. B]

a440 = Note A 4

middleC = Note C 4
