module NotesTest where


import           Chords
import           Notes
import           Strings
import           Test.HUnit

testNegation = TestCase $ assertEqual
    "transpositon is associative" (transpose (
        transpose (Note E 1) 17 ) (-17)) (Note E 1)

testPitchOrdering = TestCase $ assertEqual
    "Notes in octave ordered from C"
    ((Note C 4) < (Note Db 4)) True

testOctaveOrdering = TestCase $ assertEqual
    "Lower octaves lesser than higher octaves"
    ((Note C 0) < (Note C 1)) True

testOctavePrecedence = TestCase $ assertEqual
    "Octave dominates pitch on ordering"
    ((Note B 0) < (Note C 1)) True

testNoteOrdering = TestList [testPitchOrdering
    , testOctaveOrdering, testOctavePrecedence]


main = runTestTT $ TestList [testNegation
        , testNoteOrdering]
