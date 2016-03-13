{-
 This code was taken fromaudio processing using Haskell,
 a paper by Henning Thielemann.  I plan on using it as a
 starting point for future development.
-}


--superpose adds two signals represented by lists
superpose :: Num a => [a] -> [a] -> [a]
superpose = zipWith (+)

--static amplification of a signal
amplify :: Num a => a -> [a] -> [a]
amplify y = map (*y)

{-

-- iterate f x returns an infinite list of repeated applications of f to x: -- iterate f x == [x, f x, f (f x), ...]
iterate :: (a -> a) -> a -> [a]
iteratefx = x:iteratef(fx)
-}


--exponential...something to do with decay
exponential :: Num a => a -> [a]
exponential decay = iterate (decay *) 1

--amplitude determines the peak of a signal
amplitude :: (Num a, Ord a) => [a] -> a
amplitude x = foldl max 0 (map abs x)

--euclidean norm is the euclidean norm..?
euclideanNorm :: Floating a => [a] -> a
euclideanNorm x = sqrt (sum (map (^2) x ) )

--superposeMulti superposes multiple signals
superposeMulti :: Num a => [[a]] -> [a]
superposeMulti = foldl1 superpose

--a simple instrument sound (an oscillator with frequency
--waves per second)
oscillator :: Floating a => a -> [a]
oscillator freq = map sin (iterate (2 * pi * freq +) 0)

--a bell sound is a sine oscillator enveloped by an exponential
bell :: Floating a => a -> a -> [a]
bell decay freq = zipWith (*) (exponential decay)
                              (oscillator freq)
--processes involving feedback can be modeled with recursion
echo :: Num a => Int -> a -> [a] -> [a]
echo time gain x =
    let y = superpose x (delay time
                            (amplify gain y))
    in y

--very cool way to delay
delay :: Num a => Int -> [a] -> [a]
delay time = (replicate time 0 ++)


--echoProc allows a process to be appplied to echo
echoProc :: Num a => Int -> ([a] -> [a]) -> [a] -> [a]
echoProc time feedback x =
    let y = superpose x (delay time
                         (feedback y))
    in y


--itegrate
integrate :: Num a => a -> [a] -> [a]
itegrate = scanl (+)

--solving the inhomogeneous oscillation equation
osciODE :: Num a =>
    (a, a) -> (a, a) -> [a] -> [a]
osciODE (c0, y0) (c1, y'0) u =
    let infixl 6 .+, .-
        infixr 7 *>
        (.+) = zipWith (+)
        (.-) = zipwith (-)
        (*>) = amplify

        y = integrate y0 y'
        y' = integrate y'0 y''
        y'' = u .- (c0 *> y .+ c1 *> y')
    in y


--an example of a recursive filter, lowpass
--what is a recursive filter???
lowpass1Aupdate :: Num a => a -> a -> a -> (a, a)
lowpass1Aupdate k u0 y1 =
    let y0 = u0 + k*(y1 - u0) in (y0, y0)

lowpass1A :: Num a => a -> a -> [a] -> [a]
lowpass1A s k (u : us) =
    let (x, news) = lowpass1Aupdate k u s
    in x : lowpass1A news k us

--another lowpass example, this time using
--the State monad
lowpass1Bupdate :: Num a => a -> a -> State a a
lowpass1Bupdate k u0 =
    let update y1 =
            let y0 = u0 + k * (y1 - u0) in (y0, y0)
    in State update

lowpass1B :: Num a => a -> a -> [a] -> [a]
lowpass1B s k u = evalState
    (mapM (lowpass1Bupdate k) u) s


--Physical Units--
type Unit i = FiniteMap i Int
data PhysicsValue i a = PV a (Unit i)

instance (Eq i, Eq a) => Eq (PhysicsValue i a) where
    (PV x xu) == (PV y yu ) = x == y && xu == yu



