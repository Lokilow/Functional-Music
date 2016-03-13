import           Control.Monad   (liftM2)
import           Notes
import           Test.QuickCheck

instance Arbitrary PitchClass where
    arbitrary  =  elements [C .. B]

instance Arbitrary Note where
    arbitrary = liftM2 Note arbitrary arbitrary


testInverse :: Note -> Semitone -> Bool
testInverse (Note p o)  x = transpose
    (transpose (Note p o) x) (-x) == (Note p o)


main = quickCheck testInverse









