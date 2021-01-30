
module Brainfuck (main) where

import Data.Array as Array (Ix,Array,listArray,(!))
import Data.Word8 (Word8)
import Ram (Ram,Addr)
import System.Environment (getArgs)
import qualified Data.Ascii as Ascii (fromChar,toChar)
import qualified Ram (init,read,write)

main :: IO ()
main = do
  args <- getArgs
  let filename =
        case args of [x] -> x; _ -> error (show ("unexpected command line",args))
  prog <- readProg filename
  execute prog

readProg :: FilePath -> IO Prog
readProg path = parseProg <$> readFile path

newtype Prog = Prog { unProg :: [Op] }

data Op = Nop | Dot | Comma | Langle | Rangle | Plus | Minus | Lsquare | Rsquare
  deriving (Show)

parseProg :: String -> Prog
parseProg s = Prog [ parseOp c | c <- s ]

parseOp :: Char -> Op
parseOp = \case
  '.' -> Dot
  ',' -> Comma
  '<' -> Langle
  '>' -> Rangle
  '+' -> Plus
  '-' -> Minus
  '[' -> Lsquare
  ']' -> Rsquare
  _ -> Nop

type State = (Ram,PC,Addr)

newtype PC = PC Int
  deriving (Eq,Ord,Ix,Num,Show)

execute :: Prog -> IO ()
execute prog = loop 1 (Ram.init,0,0)
  where
    loop :: Int -> State -> IO ()
    loop i state@(_,pc,_)  =
      if not (continue pc) then pure () else do
        let op = fetch pc
        --print (i,pc,op)
        exec state op >>= loop (i+1)

    continue :: PC -> Bool
    continue pc = pc >= 0 && pc < progSize

    progSize = PC (length (unProg prog))

    fetch :: PC -> Op
    fetch = \pc -> a ! pc
      where
        a :: Array PC Op
        a = listArray (0,progSize) (unProg prog)

    exec :: State -> Op -> IO State
    exec (ram,pc,mp) = \case
      Nop -> pure (ram,pc+1,mp)
      Dot -> do put (Ram.read ram mp); pure (ram,pc+1,mp)
      Comma -> do byte <- get; pure (Ram.write ram mp byte,pc+1,mp)
      Langle -> pure (ram,pc+1,mp-1)
      Rangle -> pure (ram,pc+1,mp+1)
      Plus -> do pure (Ram.write ram mp (Ram.read ram mp + 1), pc+1,mp)
      Minus -> do pure (Ram.write ram mp (Ram.read ram mp - 1), pc+1,mp)
      Lsquare -> do
        let byte = Ram.read ram mp
        pure (if byte /= 0 then (ram, pc+1, mp) else (ram, skipRight 1 (pc+1), mp))
      Rsquare -> do
        let byte = Ram.read ram mp
        pure (if byte == 0 then (ram, pc+1, mp) else (ram, skipLeft 1 (pc-1), mp))

    skipRight :: Int -> PC -> PC
    skipRight nest pc =
      if nest == 0 then pc else
        case fetch pc of
          Lsquare -> skipRight (nest+1) (pc+1)
          Rsquare -> skipRight (nest-1) (pc+1)
          _ -> skipRight nest (pc+1)

    skipLeft :: Int -> PC -> PC
    skipLeft nest pc =
      if nest == 0 then pc+2 else
        case fetch pc of
          Lsquare -> skipLeft (nest-1) (pc-1)
          Rsquare -> skipLeft (nest+1) (pc-1)
          _ -> skipLeft nest (pc-1)

put :: Word8 -> IO ()
put w = putChar (Ascii.toChar w)

get :: IO Word8
get = do
  char <- getChar
  case Ascii.fromChar char of
    Just w -> pure w
    Nothing -> error ("get, non ascii char: " ++ show char)
