-- \/\/\/ DO NOT MODIFY THE FOLLOWING LINES \/\/\/
module ReversiAI(State,author,nickname,initial,think) where

import Reversi
-- /\/\/\ DO NOT MODIFY THE PRECEDING LINES /\/\/\

{- The board is represented as a list of 64 elements with types my piece, opponent piece or empty.
   The initial state of the board is having white pieces in position 27 and 36 while black pieces in 28 and 35. 
   The color of my AI's pieces depends on the initial state.
   Through the function think, AI will make decition based on the updated board every time after opponent making valid move.
   The function positionsToFlipInAllDirection takes one available position as argument, searches in all directions and counts those pieces which could be flipped.
   Then makes action, updates the board and continue this procedure.

   Credit about boardOriginal: Yuzhi Chen 
 -}

{- The datatype describes the state of one position on board.
   MyPiece means pieces from my side.
   OppoPiece means pieces from opponent side.
   Empty means there's no piece on this position.
-}
data PieceType = MyPiece | OppoPiece | Empty
  deriving (Eq,Show)

{- The datatype is eight different directions.
   The name is self explanatory.
-}
data Direction = Upleft | UpDirection | Upright | LeftDirection | RightDirection | Downleft | DownDirection | Downright
  deriving (Eq,Show)

{- The type Board is represented by a list of piece types.
   INVARIANT : The lenth is 64.
-}
type Board = [PieceType]

{- The internal state of your AI which is the board state.
-}
type State = Board 

author :: String
author = "Jiaying Wu"

nickname :: String
nickname = "Sniff"



{- boardOriginal
   A board with original layout, i.e., place white piece in position 27 and 36,
   place black piece in grid 28 and 35, the rest marked as empty. This function assumes that we are playing black.
   RETURNS: A board with starting layout.
   EXAMPLES: boardOriginal == [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,OppoPiece,MyPiece,Empty,Empty,Empty,Empty,Empty,Empty,MyPiece,OppoPiece,E
-}
boardOriginal :: Board
boardOriginal = (replicate 27 Empty ++ [OppoPiece,MyPiece] ++ replicate 6 Empty ++ [MyPiece,OppoPiece] ++ replicate 27 Empty)



{- positionsInDirection pStart dCheck
   Given a start position, get all of other positions in one certain direction.
   RETURNS: A list of all positions in the given direction relative to the starting position.
   EXAMPLES: positionsInDirection 37 LeftDirection == [36,35,34,33,32]
             positionsInDirection 42 Upleft == [33,24]
             positionsInDirection 42 Upright == [35,28,21,14,7]
             positionsInDirection 15 RightDirection == []
-}
positionsInDirection :: Int -> Direction -> [Int]
positionsInDirection pStart LeftDirection = [pStart - x | x <- [1 .. mod pStart 8]]
positionsInDirection pStart RightDirection = [pStart + x | x <- [1 .. 7 - mod pStart 8]]
positionsInDirection pStart UpDirection = [pStart + (-8) * x | x <- [1 .. div pStart 8]]
positionsInDirection pStart DownDirection = [pStart + 8 * x | x <- [1 .. 7 - div pStart 8]]
positionsInDirection pStart Upleft = [pStart + (-9) * x | x <- [1 .. min (mod pStart 8) (div pStart 8)]]
positionsInDirection pStart Upright = [pStart + (-7) * x | x <- [1 .. min (7 - mod pStart 8) (div pStart 8)]]
positionsInDirection pStart Downleft = [pStart + 7 * x | x <- [1 .. min (mod pStart 8) (7 - div pStart 8)]]
positionsInDirection pStart Downright = [pStart + 9 * x | x <- [1 .. min (7 - mod pStart 8) (7 - div pStart 8)]]



{- piecesInDirection board pStart dStart
   Start from one position, get the type of all pieces in a certain direction.
   RETURNS : A list of states in each position, which is pieces or empty.
   EXAMPLES: piecesInDirection boardOriginal 37 LeftDirection == [][OppoPiece,MyPiece,Empty,Empty,Empty]
             piecesInDirection boardOriginal 42 Upleft == [Empty,Empty,Empty]
             piecesInDirection boardOriginal 42 Upright == [MyPiece,MyPiece,Empty,Empty,Empty]
             piecesInDirection boardOriginal 26 RightDirection == [OppoPiece,MyPiece,Empty,Empty,Empty]
-}
piecesInDirection :: Board -> Int -> Direction -> [PieceType]
piecesInDirection board pStart dStart = [board !! index | index <- (positionsInDirection pStart dStart)]



{- countFlips list acc
   Count the number of pieces to be flipped in a list, assuming a piece is placed in front of it.
   RETURNS: The total number of pieces that need to be flipped.
   EXAMPLES: countFlips [OppoPiece, Empty, MyPiece] 0 == 0
             countFlips [OppoPiece, OppoPiece, Empty] 0 == 0
             countFlips [OppoPiece,MyPiece,Empty,Empty] 0 == 1
             countFlips [OppoPiece, OppoPiece, MyPiece, OppoPiece ] 0 == 2
-}
-- VARIANT  : length list
countFlips :: [PieceType] -> Int -> Int
countFlips [] acc = 0 -- e.g. if initial list is [OppoPiece, OppoPiece]
countFlips (x:xs) acc = if x == OppoPiece then countFlips xs (acc+1) else 
                        if x == MyPiece then acc else 0

-- pList = (piecesInDirection board pStart dStart) -- a list of all pieces' types



{- checkFlipInDirection board pCheck dCheck
   Start from one position where is planned to place a piece, check in one certain direction to see if any piece will be flipped.
   RETURNS : True if at least one piece will be flipped in the specified direction starting at the position, 
             otherwise False.
   EXAMPLES: checkFlipInDirection boardOriginal 37 LeftDirection == True
             checkFlipInDirection boardOriginal 37 Downleft == False
             checkFlipInDirection boardOriginal 42 RightDirection == False
             checkFlipInDirection boardOriginal 42 Upright == False
 -}
checkFlipInDirection :: Board -> Int -> Direction -> Bool
checkFlipInDirection board pCheck dCheck = if (countFlips (piecesInDirection board pCheck dCheck) 0) > 0 then True else False



{-checkFlipInAnyDirection board pCheck
  Start from one position where is planned to place a piece, check all directions to see if any piece will be flipped.
  RETURNS : True if there is at least one piece that will be flipped in any direction at this position,
            otherwise False.
  EXAMPLES: checkFlipInAnyDirection boardOriginal 18 == False
            checkFlipInAnyDirection boardOriginal 19 == True
            checkFlipInAnyDirection boardOriginal 20 == False
            checkFlipInAnyDirection boardOriginal 37 == True 
            checkFlipInAnyDirection boardOriginal 42 == False
-}
checkFlipInAnyDirection :: Board -> Int -> Bool
checkFlipInAnyDirection board pCheck  = elem True [checkFlipInDirection board pCheck dCheck | dCheck <- dList]

{- A list contains eight directions as elements.
-}
dList :: [Direction]
dList = Upleft : UpDirection : Upright : LeftDirection : RightDirection : Downleft : DownDirection : Downright : []



{- checkMoveValid board Reversi.Move
   Check if the specified move is vaild, i.e., the target position should be empty, within the board positions range, 
   and at least one opponent piece can be flipped after placing the target piece.
   RETURNS : True if this move is valid, otherwise False.
   EXAMPLES: checkMoveValid boardOriginal (Move 19) == True
             checkMoveValid boardOriginal (Move 20) == False
             checkMoveValid boardOriginal (Move 26) == True
             checkMoveValid boardOriginal Pass      == False
-}
checkMoveValid :: Board -> Reversi.Move -> Bool
checkMoveValid board (Move x) = (checkFlipInAnyDirection board x == True) && (x >= 0 && x <= 63) && (board !! x == Empty)
checkMoveValid board Pass = not (elem True [(checkFlipInAnyDirection board x == True) && (x >= 0 && x <= 63) && (board !! x == Empty) | x <- [0 .. 63]])



{- findMoveValid board
   Based on the current board, find all valid moves and choose the first option to do.
   RETURNS: Pass or Move to one position
   EXAMPLES : findMoveValid boardOriginal == Move 19

-}
findMoveValid :: Board -> Reversi.Move
findMoveValid board = head [moveAction | moveAction <- (Pass : [Move x | x <- [0..63]]), checkMoveValid board moveAction]

-- a list contains all moves including pass
--Pass : [Move x | x <- [0..63]] 

-- a list of all valid moveActions from a list contains all moves
--[moveAction | moveAction <- (Pass : [Move x | x <- [0..63]]), checkMoveValid board moveAction]
--[Move 19,Move 26,Move 37,Move 44]



{- positionsToFlipInDirection board pCheck dCheck
   Given the current board and the next-placed position, construct a list of positions where 
   the piece to be replaced in the direction specified by the position.
   RETURNS: A list that contains positions to be flipped to my color in the specified direction.
   EXAMPLES: positionsToFlipInDirection boardOriginal 19 DownDirection == [27]
             positionsToFlipInDirection boardOriginal  37 LeftDirection == [36]
             positionsToFlipInDirection boardOriginal  37 Downright == []
-}
positionsToFlipInDirection :: Board -> Int -> Direction -> [Int]
positionsToFlipInDirection board pCheck dCheck = take (countFlips (piecesInDirection board pCheck dCheck) 0) (positionsInDirection pCheck dCheck)

--  take the plist [27,35]
    -- take 2 [27,35, 43, 51, 59]
      -- 2 is from : ((countFlips list acc) + 1)
         -- the list is from : piecesInDirection board pStart dStart
      -- plist is from : positionsInDirection pStart dCheck



{- positionsToFlipInAllDirection board pCheck
   Given the current board and the next-placed position, construct a list of positions where 
   the piece to be replaced in all directions.
   RETURNS: A list that contains positions to be flipped to my color in all directions.
   EXAMPLES: positionsToFlipInAllDirection boardOriginal 19 == [27]
-}
positionsToFlipInAllDirection :: Board -> Int -> [Int]
positionsToFlipInAllDirection board pCheck = foldl (++) [] [(positionsToFlipInDirection board pCheck dCheck) | dCheck <- dList]



{- makeAction board moveAction
    Make a vaild move, either place a piece and flip opponent piece or pass.
    RETURNS: A board after the move is played.
    EXAMPLES : makeAction boardOriginal (Move 19) == [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,MyPiece,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,MyPiece,MyPiece,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,MyPiece,OppoPiece,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty] 

               makeAction boardOriginal Pass      == [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,OppoPiece,MyPiece,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,MyPiece,OppoPiece,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                      Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]

-}
makeAction :: Board -> Reversi.Move -> Board
makeAction board (Move x) =
                [if not (elem position (positionsToFlipInAllDirection board x)) && position /= x
                    then board !! position
                    else MyPiece 
                | position <- [0..63]] 

makeAction board Pass = [board !! position | position <- [0..63]]


-- if position exists in the list from positionsToFlipInAllDirection
  -- the list : positionsToFlipInAllDirection board x
-- elem position (positionsToFlipInAllDirection board x) then 


{- initial player
   Set up initial state, which depends on the color played.
   RETURNS : A board in initial state.
   EXAMPLES : initial Black == [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                Empty,Empty,Empty,OppoPiece,MyPiece,Empty,Empty,Empty,
                                Empty,Empty,Empty,MyPiece,OppoPiece,Empty,Empty,Empty,
                                Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]
 -}
initial :: Reversi.Player -> State
initial Black = boardOriginal
initial White = switchPlayer boardOriginal



{- switchPlayer board
  Switch player by setting piece type to the oppsite side.
  RETURNS  : A board where my pieces flipped to opponent's, opponent pieces flipped to mine.
  EXAMPLES : switchPlayer (initial Black) == [Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                              Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                              Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                              Empty,Empty,Empty,MyPiece,OppoPiece,Empty,Empty,Empty,
                                              Empty,Empty,Empty,OppoPiece,MyPiece,Empty,Empty,Empty,
                                              Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                              Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                              Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty]
          -}
switchPlayer :: Board -> Board
switchPlayer board = [if (board !! position) == MyPiece then OppoPiece 
                         else if (board !! position) == OppoPiece then MyPiece
                         else Empty
                     | position <- [0..63]]


{- think beforeOppoTurn oppoAction timeLeft
   It makes decision basically based on the current state of my AI and opponent's last move.
   RETURNS  : A tuple with the action of my AI (move to a position or pass) and the state of board after making that move.
   EXAMPLES : think init (Move 37) 0 == (Move 20,[Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                  Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                  Empty,Empty,Empty,Empty,MyPiece,Empty,Empty,Empty,
                                                  Empty,Empty,Empty,MyPiece,MyPiece,Empty,Empty,Empty,
                                                  Empty,Empty,Empty,OppoPiece,MyPiece,Empty,Empty,Empty,
                                                  Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                  Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty,
                                                  Empty,Empty,Empty,Empty,Empty,Empty,Empty,Empty])
 -}
think :: State -> Reversi.Move -> Double -> (Reversi.Move, State)
think beforeOppoTurn oppoAction timeLeft = 
          let beforeMyTurn = switchPlayer (makeAction (switchPlayer beforeOppoTurn) oppoAction)
          in (findMoveValid beforeMyTurn, makeAction beforeMyTurn (findMoveValid beforeMyTurn))


-- after opponent played 
   -- switchPlayer (makeAction (switchPlayer beforeOppoTurn) oppoAction)
-- my turn to play
   -- makeAction board myAction
      -- myAction is from : findMoveValid beforeMyTurn
-- return (myAction, afterMyTurn)




