module Brainfuck
    ( runBrainfuck
    ) where

import System.IO
import System.Exit
import System.Environment
import Data.Char
import Control.Monad

{- Bidirectional tapes have a list of what's ahead and a list of what's behind -}
data BidirectionalTape a = BidirectionalTape [a] [a]
{- Program memory is a bidirectional tape made of characters -}
type ProgramMemory = BidirectionalTape Char
{- Data memory is a bidirectional tape made of integers -}
type DataMemory = BidirectionalTape Integer

{- Interactions specify the IO of a single cycle.
 - They comprise the impure part of the interpreter.
 - -}
data Interaction = NoInteraction | Request | Offer | Termination

{- Context specifies each state of the interpreter.
 - It contains the Program Memory, the Data Memory, as well as the IO descriptor.
 - -}
data Context = Context ProgramMemory DataMemory Interaction

{- Execution Order helper function -}
convert :: (Integral a) => Char -> a
convert '[' = 1
convert ']' = (-1)
convert _ = 0

{- Skip ahead to matching closed brackets -}
skipAhead :: ProgramMemory -> ProgramMemory
skipAhead (BidirectionalTape [] bhd) = BidirectionalTape [] bhd
skipAhead (BidirectionalTape ahd bhd) = BidirectionalTape newAhead newBehind
    where valuedAhead = map convert ahd
          paths = map (\n -> (sum (take n valuedAhead),n)) [1..]
          length = snd $ head $ dropWhile (\(x,_) -> x /= 0) paths
          newAhead = drop length ahd
          leftBehind = reverse $ take length ahd
          newBehind = leftBehind ++ bhd
          
{- Skip back to matching opening brackets -}
skipBack :: ProgramMemory -> ProgramMemory
skipBack (BidirectionalTape ahd []) = BidirectionalTape ahd []
skipBack (BidirectionalTape ahd bhd) = BidirectionalTape newAhead newBehind
    where valuedBehind = map convert bhd
          paths = map (\n -> (sum (take n valuedBehind),n)) [1..]
          length = snd $ head $ dropWhile (\(x,_) -> x /= 1) paths
          newBehind = drop length bhd
          sentAhead = reverse $ take length bhd
          newAhead = sentAhead ++ ahd
-- Pitfall: It has to have a net balance of 1 rather than 0 because the head
--      of the Ahead list is the current program counter

{- The brain of the operation -}
brainfuck :: Context -> Context

-- Code terminated
brainfuck state@(Context _ _ Termination) = state
-- IO data to be processed
brainfuck state@(Context _ _ Request) = state
brainfuck state@(Context _ _ Offer) = state
-- No more instructions in program tape
brainfuck (Context programTape@(BidirectionalTape [] _) dataTape _) =
    Context programTape dataTape Termination

---- Regular Data Tape operations
-- Move Data Tape ahead
    -- When there's no data tape ahead
brainfuck (Context (BidirectionalTape ('>':ahead) behind) (BidirectionalTape [] dataBehind) _) =
    Context (BidirectionalTape ahead ('>':behind)) (BidirectionalTape [] (0:dataBehind)) NoInteraction
    -- Otherwise
brainfuck (Context (BidirectionalTape ('>':ahead) behind) (BidirectionalTape (dataCell:dataAhead) dataBehind) _) =
    Context (BidirectionalTape ahead ('>':behind)) (BidirectionalTape dataAhead (dataCell:dataBehind)) NoInteraction
-- Move Data Tape behind
    -- When there's no data tape behind
brainfuck (Context (BidirectionalTape ('<':ahead) behind) (BidirectionalTape dataAhead []) _) =
    Context (BidirectionalTape ahead ('<':behind)) (BidirectionalTape (0:dataAhead) []) NoInteraction
    -- Otherwise
brainfuck (Context (BidirectionalTape ('<':ahead) behind) (BidirectionalTape dataAhead (dataCell:dataBehind)) _) =
    Context (BidirectionalTape ahead ('<':behind)) (BidirectionalTape (dataCell:dataAhead) dataBehind) NoInteraction
-- Increment Data Cell
    -- When there's no data tape ahead
brainfuck (Context (BidirectionalTape ('+':ahead) behind) (BidirectionalTape [] dataBehind) _) =
    Context (BidirectionalTape ahead ('+':behind)) (BidirectionalTape [1] dataBehind) NoInteraction
    -- Otherwise
brainfuck (Context (BidirectionalTape ('+':ahead) behind) (BidirectionalTape (dataCell:dataAhead) dataBehind) _) =
    Context (BidirectionalTape ahead ('+':behind)) (BidirectionalTape ((dataCell + 1):dataAhead) dataBehind) NoInteraction
-- Decrement Data Cell
    -- When there's no data tape ahead
brainfuck (Context (BidirectionalTape ('-':ahead) behind) (BidirectionalTape [] dataBehind) _) =
    Context (BidirectionalTape ahead ('-':behind)) (BidirectionalTape [(-1)] dataBehind) NoInteraction
    -- Otherwise
brainfuck (Context (BidirectionalTape ('-':ahead) behind) (BidirectionalTape (dataCell:dataAhead) dataBehind) _) =
    Context (BidirectionalTape ahead ('-':behind)) (BidirectionalTape ((dataCell - 1):dataAhead) dataBehind) NoInteraction

---- Program Tape Operations
-- Skip Ahead
    -- When there's no data tape ahead
brainfuck (Context programTape@(BidirectionalTape ('[':_) _) dataTape@(BidirectionalTape [] _) _) =
    Context (skipAhead programTape) dataTape NoInteraction
    -- When the data's zero
brainfuck (Context programTape@(BidirectionalTape ('[':_) _) dataTape@(BidirectionalTape (0:_) _) _) =
    Context (skipAhead programTape) dataTape NoInteraction
    -- Otherwise
brainfuck (Context (BidirectionalTape ('[':ahead) behind) dataTape _) =
    Context (BidirectionalTape ahead ('[':behind)) dataTape NoInteraction
-- Skip Back
    -- When there's no data tape ahead
brainfuck (Context (BidirectionalTape (']':ahead) behind) dataTape@(BidirectionalTape [] _) _) =
    Context (BidirectionalTape ahead (']':behind)) dataTape NoInteraction
    -- When the data's zero
brainfuck (Context (BidirectionalTape (']':ahead) behind) dataTape@(BidirectionalTape (0:_) _) _) =
    Context (BidirectionalTape ahead (']':behind)) dataTape NoInteraction
    -- Otherwise
brainfuck (Context programTape@(BidirectionalTape (']':_) _) dataTape _) =
    Context (skipBack programTape) dataTape NoInteraction

---- IO Operations
    {- IO Operations require impure intervention from the IO side of the interpreter -}
-- Output
brainfuck (Context (BidirectionalTape ('.':ahead) behind) dataTape _) =
    Context (BidirectionalTape ahead ('.':behind)) dataTape Offer
-- Input
brainfuck (Context (BidirectionalTape (',':ahead) behind) dataTape _) =
    Context (BidirectionalTape ahead (',':behind)) dataTape Request

---- Comments
    {- All code that is not part of the instruction set is considered a comment -}
brainfuck (Context (BidirectionalTape (nop:ahead) behind) dataTape _) =
    Context (BidirectionalTape ahead (nop:behind)) dataTape NoInteraction


{- Impure handling of the loop -}
loop :: Context -> IO ()
-- When program has terminated, quit
loop state@(Context _ _ Termination) = return ()
---- IO data to be processed
-- data request
    -- When there's no data tape ahead
loop (Context programTape (BidirectionalTape [] behind) Request) = do
    input <- getChar
    let nextState = Context programTape (BidirectionalTape ((fromIntegral (ord input)):[]) behind) NoInteraction
    loop nextState
    -- Otherwise
loop (Context programTape (BidirectionalTape (_:ahead) behind) Request) = do
    input <- getChar
    let nextState = Context programTape (BidirectionalTape ((fromIntegral (ord input)):ahead) behind) NoInteraction
    loop nextState
-- data request
    -- When there's no data tape ahead
loop (Context programTape dataTape@(BidirectionalTape [] _) Offer) = do
    let output = chr 0
    putChar output
    let nextState = Context programTape dataTape NoInteraction
    loop nextState
    -- Otherwise
loop (Context programTape dataTape@(BidirectionalTape (c:_) _) Offer) = do
    let output = chr $ fromIntegral c
    putChar output
    let nextState = Context programTape dataTape NoInteraction
    loop nextState
---- Regular execution
loop state = do
    let nextState = head $ dropWhile noInteraction $ iterate brainfuck state
    loop nextState
    where noInteraction :: Context -> Bool
          noInteraction (Context _ _ NoInteraction) = True
          noInteraction _ = False

{- Read CLI arguments -}
runBrainfuck :: IO ()
runBrainfuck = do
    -- No buffering, so IO works correctly
    hSetBuffering stdout NoBuffering
    args <- getArgs
    -- If there are no arguments, or if there are too many, program fails
    when (length args /= 1) exitFailure
    -- Read forward program tape
    program <- readFile (args!!0)
    let initialState = Context (BidirectionalTape program []) (BidirectionalTape [] []) NoInteraction
    loop initialState
