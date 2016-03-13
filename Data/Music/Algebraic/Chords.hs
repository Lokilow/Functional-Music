module Chords where

import           Notes


data Chord = Chord [Note]
    deriving Show



majorChord ::  Note -> Chord
majorChord r = Chord [r, (transpose r 4), (transpose r 7)]


minorChord ::  Note -> Chord
minorChord r = Chord [r, (transpose r 3), (transpose r 7)]

seventh :: Note -> Chord
seventh r = Chord [r, (transpose r 4), (transpose r 7)
                  , (transpose r 10)]

major7 :: Note -> Chord
major7 r = Chord [r, (transpose r 4), (transpose r 7)
                 , (transpose r 11)]

minor7 :: Note -> Chord
minor7 r = Chord [r, (transpose r 3), (transpose r 7)
                 , (transpose r 10)]

hendrix :: Note -> Chord
hendrix r = Chord [r, (transpose r 4), (transpose r 7)
                  , (transpose r 15)]

add9 :: Note -> Chord
add9 r = Chord [r, (transpose r 4), (transpose r 7)
               , (transpose r 10), (transpose r 14)]


type Root = Note
type ChordProgression = [Chord]
a440 = Note A 4

pachelbel :: Note -> ChordProgression
pachelbel r = [(majorChord r), (majorChord $ (transpose r 7))
              , (minorChord $ (transpose r 9))
              , (majorChord $ (transpose r 5))]


