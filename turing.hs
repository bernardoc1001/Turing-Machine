-- Section 1

-- String gives the tape contents, and the Int gives the current position of the read-write head on the tape
type Tape = (String, Int)


-- Int is the current state (this should be the initial state for the starting configuration), and the Tape is the current value of the tape. 
type Configuration = (Int, Tape)

data Action = L | R | Y | N
              deriving Eq

-- input (Int,Char) pair gives the current state and the current tape symbol, and the output (Int,Char,Action)triple gives the new state, new tape symbol and resulting action
type Transitions = (Int, Char) -> (Int, Char, Action)

data Result = Accept Tape | Reject Tape
              deriving Show

--run :: Configuration -> Transitions -> Result


-- Helper functions
getTapeChar :: Tape -> Char
getTapeChar (xs , index) = xs !! index

moveTapeHead :: Tape -> Action -> Tape
moveTapeHead (xs, index) a
  |a == L    = (xs, (index -1))
  |a == R    = (xs, (index + 1))
  |otherwise = error "A move action was expected, received a result Action"

giveResult :: Tape -> Action -> Result
giveResult t a
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

