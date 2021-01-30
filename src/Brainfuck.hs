
module Brainfuck (main) where

import Control.Monad (ap,liftM)
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
    where execute = if True then execute2 else execute1

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

execute1 :: Prog -> IO ()
execute1 prog = loop 1 (Ram.init,0,0)
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
      Dot -> do putW8 (Ram.read ram mp); pure (ram,pc+1,mp)
      Comma -> do byte <- getW8; pure (Ram.write ram mp byte,pc+1,mp)
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

putW8 :: Word8 -> IO ()
putW8 w = putChar (Ascii.toChar w)

getW8 :: IO Word8
getW8 = do
  char <- getChar
  case Ascii.fromChar char of
    Just w -> pure w
    Nothing -> error ("get, non ascii char: " ++ show char)


----------------------------------------------------------------------
-- restructure interpreter to go via effects

instance Functor Eff where fmap = liftM
instance Applicative Eff where pure = return; (<*>) = ap
instance Monad Eff where return = Ret; (>>=) = Bind

execute2 :: Prog -> IO ()
execute2 prog = loop 1 (Ram.init,0,0)
  where
    loop :: Int -> State -> IO ()
    loop i state@(_,pc,_)  =
      if not (continue pc) then pure () else do
        --print (i,pc,op)
        interpret fetch semantics state >>= loop (i+1)

    continue :: PC -> Bool
    continue pc = pc >= 0 && pc < progSize

    progSize = PC (length (unProg prog))

    fetch :: PC -> Op
    fetch = \pc -> a ! pc
      where
        a :: Array PC Op
        a = listArray (0,progSize) (unProg prog)

interpret :: (PC -> Op) -> Eff () -> State -> IO State
interpret fetch eff s0 = loop s0 (\() s -> pure s) eff
  where
    loop :: State -> (a -> State -> IO State) -> Eff a -> IO State
    loop s@(ram,pc,mp) k = \case
      Ret a -> k a s
      Bind e f -> loop s (\a s -> loop s k (f a))  e
      GetPC -> k pc s
      SetPC pc -> k () (ram,pc,mp)
      GetMP -> k mp s
      SetMP mp -> k () (ram,pc,mp)
      GetByte -> do b <- getW8; k b s
      PutByte b -> do putW8 b; k () s
      ReadMem a -> k (Ram.read ram a) s
      WriteMem a b -> k () (Ram.write ram a b, pc, mp)
      Fetch pc -> k (fetch pc) s
      IsZero b -> k (b == 0) s

data Eff a where
  Ret :: a -> Eff a
  Bind :: Eff a -> (a -> Eff b) -> Eff b
  GetPC :: Eff PC
  SetPC :: PC -> Eff ()
  Fetch :: PC -> Eff Op
  GetMP :: Eff Addr
  SetMP :: Addr -> Eff ()
  GetByte :: Eff Word8
  PutByte :: Word8 -> Eff ()
  ReadMem :: Addr -> Eff Word8
  WriteMem :: Addr -> Word8 -> Eff ()
  IsZero :: Word8 -> Eff Bool

semantics :: Eff ()
semantics = do
  pc <- GetPC
  op <- Fetch pc
  SetPC (pc+1)
  semanticsOp op

semanticsOp :: Op -> Eff ()
semanticsOp = \case
  Nop -> pure ()
  Dot -> do
    a <- GetMP
    b <- ReadMem a
    PutByte b
  Comma -> do
    b <- GetByte
    a <- GetMP
    WriteMem a b
  Langle -> do
    a <- GetMP
    SetMP (a-1)
  Rangle -> do
    a <- GetMP
    SetMP (a+1)
  Plus -> do
    a <- GetMP
    b <- ReadMem a
    WriteMem a (b+1)
  Minus -> do
    a <- GetMP
    b <- ReadMem a
    WriteMem a (b-1)
  Lsquare -> do
    a <- GetMP
    b <- ReadMem a
    IsZero b >>= \case
      False -> pure ()
      True -> do
        pc <- GetPC
        pc <- skipRight 1 pc
        SetPC pc
  Rsquare -> do
    a <- GetMP
    b <- ReadMem a
    IsZero b >>= \case
      True -> pure ()
      False -> do
        pc <- GetPC
        pc <- skipLeft 1 (pc-2)
        SetPC (pc+2)

skipRight :: Int -> PC -> Eff PC
skipRight nest pc =
  if nest == 0 then pure pc else do
    op <- Fetch pc
    case op of
      Lsquare -> skipRight (nest+1) (pc+1)
      Rsquare -> skipRight (nest-1) (pc+1)
      _ -> skipRight nest (pc+1)

skipLeft :: Int -> PC -> Eff PC
skipLeft nest pc =
  if nest == 0 then pure pc else do
    op <- Fetch pc
    case op of
      Lsquare -> skipLeft (nest-1) (pc-1)
      Rsquare -> skipLeft (nest+1) (pc-1)
      _ -> skipLeft nest (pc-1)
