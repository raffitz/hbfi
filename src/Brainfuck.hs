module Brainfuck
    ( runBrainfuck
    ) where

import System.Exit
import System.Environment
import Data.Char
import Control.Monad

skipAhead :: String -> (String,String)
skipAhead [] = ([],[])
skipAhead str = skipAhead' str "" 1
    where skipAhead' "" bhd _ = ("",bhd)
          skipAhead' ahd bhd 0 = (ahd,bhd)
          skipAhead' (']':ahd) bhd 1 = (ahd,(']':bhd))
          skipAhead' (']':ahd) bhd n = skipAhead' ahd (']':bhd) (n-1)
          skipAhead' ('[':ahd) bhd n = skipAhead' ahd ('[':bhd) (n+1)
          skipAhead' (a:ahd) bhd n = skipAhead' ahd (a:bhd) n

skipBack :: String -> (String,String)
skipBack [] = ([],[])
skipBack str = skipBack' str "" 1
    where skipBack' "" bhd _ = ("",bhd)
          skipBack' ahd bhd 0 = (ahd,bhd)
          skipBack' ('[':ahd) bhd 1 = (ahd,'[':bhd)
          skipBack' ('[':ahd) bhd n = skipBack' ahd ('[':bhd) (n-1)
          skipBack' (']':ahd) bhd n = skipBack' ahd (']':bhd) (n+1)
          skipBack' (a:ahd) bhd n = skipBack' ahd (a:bhd) n

brainfuck :: Integral a => (String,String,[a],[a],Maybe (Maybe Char)) -> (String,String,[a],[a],Maybe (Maybe Char))
brainfuck state@([],behind,memAhead,memBehind,io)=state
brainfuck (('>':ahead),behind,[],memBehind,_) = (ahead,'>':behind,[],0:memBehind,Just Nothing)
brainfuck (('>':ahead),behind,cell:memAhead,memBehind,_) = (ahead,'>':behind,memAhead,cell:memBehind,Just Nothing)
brainfuck (('<':ahead),behind,memAhead,[],_) = (ahead,'<':behind,0:memAhead,[],Just Nothing)
brainfuck (('<':ahead),behind,memAhead,cell:memBehind,_) = (ahead,'<':behind,cell:memAhead,memBehind,Just Nothing)
brainfuck (('+':ahead),behind,[],memBehind,_) = (ahead,'+':behind,[1],memBehind,Just Nothing)
brainfuck (('+':ahead),behind,cell:memAhead,memBehind,_) = (ahead,'+':behind,(cell + 1):memAhead,memBehind,Just Nothing)
brainfuck (('-':ahead),behind,[],memBehind,_) = (ahead,'-':behind,[(-1)],memBehind,Just Nothing)
brainfuck (('-':ahead),behind,cell:memAhead,memBehind,_) = (ahead,'-':behind,(cell - 1):memAhead,memBehind,Just Nothing)
brainfuck (('.':ahead),behind,[],memBehind,_) = (ahead,'.':behind,[],memBehind,Just (Just (chr 0)))
brainfuck (('.':ahead),behind,cell:memAhead,memBehind,_) = (ahead,'.':behind,cell:memAhead,memBehind,Just (Just (chr (fromIntegral cell))))
brainfuck ((',':ahead),behind,[],memBehind,Just (Just char)) = (ahead,',':behind,[(fromIntegral (ord char))],memBehind,Just Nothing)
brainfuck ((',':ahead),behind,_:memAhead,memBehind,Just (Just char)) = (ahead,',':behind,(fromIntegral (ord char)):memAhead,memBehind,Just Nothing)
brainfuck ((',':ahead),behind,memAhead,memBehind,_) = (',':ahead,behind,memAhead,memBehind,Nothing)
brainfuck (('[':ahead),behind,[],memBehind,_) = (fst skipped,(snd skipped) ++ ('[':behind),[],memBehind,Just Nothing)
    where skipped = skipAhead ahead
brainfuck (('[':ahead),behind,0:memAhead,memBehind,_) = (fst skipped,(snd skipped) ++ ('[':behind),0:memAhead,memBehind,Just Nothing)
    where skipped = skipAhead ahead
brainfuck (('[':ahead),behind,memAhead,memBehind,_) = (ahead,'[':behind,memAhead,memBehind,Just Nothing)
brainfuck ((']':ahead),behind,[],memBehind,io) = (ahead,']':behind,[],memBehind,Just Nothing)
brainfuck ((']':ahead),behind,0:memAhead,memBehind,io) = (ahead,']':behind,0:memAhead,memBehind,Just Nothing)
brainfuck ((']':ahead),behind,memAhead,memBehind,io) = ((snd skipped) ++ (']':ahead),fst skipped,memAhead,memBehind,Just Nothing)
    where skipped = skipBack behind
brainfuck ((x:ahead),behind,memAhead,memBehind,io) = (ahead,x:behind,memAhead,memBehind,Just Nothing)

loop :: Integral a => (String,String,[a],[a],Maybe (Maybe Char)) -> IO ()
loop ([],_,_,_,_) = return ()
loop (ahead,behind,memAhead,memBehind,Nothing) = do
    char <- getChar
    let state = brainfuck (ahead,behind,memAhead,memBehind,Just (Just char))
    loop state
loop (ahead,behind,memAhead,memBehind,Just (Just char)) = do
    putChar char
    let state = brainfuck (ahead,behind,memAhead,memBehind,Just Nothing)
    loop state
loop state = do
    let newstate = brainfuck state
    loop newstate

runBrainfuck :: IO ()
runBrainfuck = do
    args <- getArgs
    when (length args /= 1) exitFailure
    program <- readFile (args!!0)
    loop (program,"",[],[],Just Nothing)
