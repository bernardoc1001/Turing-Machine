-- Section 1

-- String gives the tape contents, and the Int gives the current position of the read-write head on the tape
type Tape = (String, Int)


-- Int is the current state (this should be the initial state for the starting configuration), and the Tape is the current value of the tape. 
type Configuration = (Int, Tape)

data Action = L | R | Y | N
              deriving (Eq, Show)

-- input (Int,Char) pair gives the current state and the current tape symbol, and the output (Int,Char,Action)triple gives the new state, new tape symbol and resulting action
type Transitions = (Int, Char) -> (Int, Char, Action)

data Result = Accept Tape | Reject Tape
              deriving Show



run :: Configuration -> Transitions -> Result
run (s,t) transition
  | a == L || a == R = run (state,moveTape) transition
  | otherwise        = getResult resultTape a
    where tran       = getTransition transition (s,(getTapeChar t))
          state      = fstTripleElem tran
          char       = sndTripleElem tran
          a          = thrdTripleElem tran
          moveTape   = moveTapeHead (setTapeChar t char) a
          resultTape = (setTapeChar t char)


-- Helper functions
getTapeChar :: Tape -> Char
getTapeChar (xs , index) = xs !! index

getTransition :: Transitions -> (Int, Char) -> (Int, Char, Action)
getTransition tran t = (tran t)

fstTripleElem :: (t1, t2, t3) -> t1
fstTripleElem (x,_,_) = x

sndTripleElem :: (t1, t2, t3) -> t2
sndTripleElem (_,x,_) = x

thrdTripleElem :: (t1, t2, t3) -> t3
thrdTripleElem (_,_,x) = x

moveTapeHead :: Tape -> Action -> Tape
moveTapeHead (xs, index) a
  |a == L    = (xs, (index -1))
  |a == R    = (xs, (index + 1))
  |otherwise = error "A move action was expected, received a result Action"

getResult :: Tape -> Action -> Result
getResult t a
  |a == Y    = Accept t
  |a == N    = Reject t
  |otherwise = error "A result action was expected, received a move Action"
  
setTapeChar :: Tape -> Char -> Tape
setTapeChar ([], _) _ = error "Tape can't be completly Empty"
setTapeChar (_, 0) _ = error "Can't overwrite left delimiter"
setTapeChar (xs, index) c = ((replaceNthElem xs index c), index)


replaceNthElem :: String -> Int -> Char -> String
replaceNthElem [] index c
  |index == 0 = [c]
  |otherwise  = error "Index out of bounds"
replaceNthElem (x:xs) index c
  |index == 0 = c:xs
  |otherwise  = x : replaceNthElem xs (index - 1) c



-- RULES
anbncn (s,c) = case (s,c) of
                  (0,' ') -> (0,' ',Y)
                  (0,'a') -> (1,'X',R)
                  (0,'b') -> (0,'b',N)
                  (0,'c') -> (0,'c',N)
                  (0,'X') -> (0,'X',N)
                  (0,'Y') -> (0,'Y',R)
                  (0,'Z') -> (0,'Z',R)
                  (1,' ') -> (1,' ',N)
                  (1,'a') -> (1,'a',R)
                  (1,'b') -> (2,'Y',R)
                  (1,'c') -> (1,'c',N)
                  (1,'X') -> (1,'X',N)
                  (1,'Y') -> (1,'Y',R)
                  (1,'Z') -> (1,'Z',N)
                  (2,' ') -> (2,' ',N)
                  (2,'a') -> (2,'a',N)
                  (2,'b') -> (2,'b',R)
                  (2,'c') -> (3,'Z',L)
                  (2,'X') -> (2,'X',N)
                  (2,'Y') -> (2,'Y',N)
                  (2,'Z') -> (2,'Z',R)
                  (3,' ') -> (3,' ',N)
                  (3,'a') -> (3,'a',L)
                  (3,'b') -> (3,'b',L)
                  (3,'c') -> (3,'c',N)
                  (3,'X') -> (0,'X',R)
                  (3,'Y') -> (3,'Y',L)
                  (3,'Z') -> (3,'Z',L)
                  _       -> error "No valid transition"
