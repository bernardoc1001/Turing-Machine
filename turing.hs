-- Section 1

type Tape = (String, Int)

type Configuration = (Int, Tape)

data Action = L | R | Y | N
              deriving (Eq, Show)

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



--------------------- RULES -----------------------------------------
-- Q2
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

-- Q3
palindrome(s,c) = case (s,c) of
                    (0,' ') -> (0,' ',Y) --Completed Successfully
                    (0,'X') -> (0,'X',Y) --Completed Successfully
                    (0,'Y') -> (0,'Y',Y) --Completed Successfully
                    (0,'a') -> (1,'X',R) --STATE 1 = Found A at start
                    (0,'b') -> (2,'Y',R) --STATE 2 = Found B at start

                    --A Search
                    --Search for end of remaining input after finding an A
                    (1,'a') -> (1,'a',R)
                    (1,'b') -> (1,'b',R)
                    (1,' ') -> (3,' ',L) --Found end, move left 1
                    (1,'X') -> (3,'X',L) --Found end, move left 1
                    (1,'Y') -> (3,'Y',L) --Found end, move left 1

                    --Check if character is an A
                    (3,'a') -> (4,'X',L) --Found A, replace with X and search for the start
                    (3,'b') -> (3,'b',N) --Found B, reject
                    (3,'X') -> (3,'X',Y) --Accept the case for a single A input
                    (3,'Y') -> (3,'Y',N) --Something weird has happened, reject (double check if this is needed)

                    -- Multipurpose search for start
                    --Searching for start of remaining input
                    (4,'a') -> (4,'a',L) --Keep Looking
                    (4,'b') -> (4,'a',L) --Keep Looking
                    (4,'X') -> (0,'X',R) --Found Start, start checking the next letter
                    (4,'Y') -> (0,'Y',R) --Found Start, start checking the next letter

                    -- B Search
                    --Search for end of remaining input after finding a B
                    (2,'a') -> (2,'a',R)
                    (2,'b') -> (2,'b',R)
                    (2,' ') -> (5,' ',L) --Found end, move left 1
                    (2,'X') -> (5,'X',L) --Found end, move left 1
                    (2,'Y') -> (5,'Y',L) --Found end, move left 1

                    --Check if character is a B
                    (5,'b') -> (4,'Y',L) --Found B, replace with Y and search for the start
                    (5,'a') -> (5,'a',N) --Found A, reject
                    (5,'Y') -> (5,'Y',Y) --Accept the case for a single B input
                    (5,'X') -> (5,'X',N) --Something weird has happened, reject (double check if this is needed)
                    
                    _      -> error "No Valid Transition"

