
module Brainfuck (main, execute2, execute3) where

import Control.Monad (ap,liftM)
import Data.Array as Array (Ix,Array,listArray,(!))
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Word8 (Word8)
import Ram (Ram,Addr)
import System.Environment (getArgs)
import System.FilePath (takeBaseName)
import System.IO (isEOF)
import Text.Read (readMaybe)
import qualified Data.Ascii as Ascii (fromChar,toChar)
import qualified Data.Map.Strict as Map
import qualified Ram (init,read,write)

main :: IO ()
main = do
  args <- getArgs
  let Config {filename,version} = parseArgs args
  prog <- readProg filename
  let tag = takeBaseName filename
  let execute =
        case version of
          1 -> execute1 -- basic interpreter
          2 -> execute2 -- interpreter (via effects)
          3 -> execute3 tag True -- interpreter (via effects, and residual code
          4 -> execute3 tag False -- generate c-code from residual
          v -> error (show ("unknown version",v))
  execute prog

data Config = Config { filename :: FilePath, version :: Int }

parseArgs :: [String] -> Config
parseArgs args =
  fromMaybe (error (show ("unexpected command line",args))) $
  case args of
    [filename] -> pure $ Config { filename, version = 3 } -- default
    [n,filename] -> do
      version <- readMaybe @Int n
      pure $ Config { filename, version }
    _ -> Nothing

readProg :: FilePath -> IO Prog
readProg path = parseProg <$> readFile path

newtype Prog = Prog { unProg :: [Op] } deriving Show

data Op = Nop | Dot | Comma | Langle | Rangle | Plus | Minus | Lsquare | Rsquare
  deriving (Eq,Show)

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
  deriving (Eq,Ord,Ix,Num,Enum,Show)

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
  isEOF >>= \case
    True -> pure 0
    False -> do
      char <- getChar
      case Ascii.fromChar char of
        Just w -> pure w
        Nothing -> error ("get, non ascii char: " ++ show char)

----------------------------------------------------------------------
-- Semantics defined w.r.t Effects

data Eff a b x where
  Ret :: x -> Eff a b x
  Bind :: Eff a b x -> (x -> Eff a b y) -> Eff a b y
  GetPC :: Eff a b PC
  SetPC :: PC -> Eff a b ()
  Fetch :: PC -> Eff a b Op
  GetMP :: Eff a b a
  SetMP :: a -> Eff a b ()
  GetByte :: Eff a b b
  PutByte :: b -> Eff a b ()
  ReadMem :: a -> Eff a b b
  WriteMem :: a -> b -> Eff a b ()
  IsZero :: b -> Eff a b Bool
  AddToByte :: b -> Word8 -> Eff a b b
  OffsetAddr :: a -> Int -> Eff a b a

theSemantics :: Eff a b ()
theSemantics = do
  pc <- GetPC
  op <- Fetch pc
  SetPC (pc+1)
  semanticsOp op

semanticsOp :: Op -> Eff a b ()
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
    a' <- OffsetAddr a (-1)
    SetMP a'
  Rangle -> do
    a <- GetMP
    a' <- OffsetAddr a 1
    SetMP a'
  Plus -> do
    a <- GetMP
    b <- ReadMem a
    b' <- AddToByte b 1
    WriteMem a b'
  Minus -> do
    a <- GetMP
    b <- ReadMem a
    b' <- AddToByte b 255
    WriteMem a b'
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

skipRight :: Int -> PC -> Eff a b PC
skipRight nest pc =
  if nest == 0 then pure pc else do
    op <- Fetch pc
    case op of
      Lsquare -> skipRight (nest+1) (pc+1)
      Rsquare -> skipRight (nest-1) (pc+1)
      _ -> skipRight nest (pc+1)

skipLeft :: Int -> PC -> Eff a b PC
skipLeft nest pc =
  if nest == 0 then pure pc else do
    op <- Fetch pc
    case op of
      Lsquare -> skipLeft (nest-1) (pc-1)
      Rsquare -> skipLeft (nest+1) (pc-1)
      _ -> skipLeft nest (pc-1)

----------------------------------------------------------------------
-- restructure interpreter to go via effects

instance Functor (Eff a b) where fmap = liftM
instance Applicative (Eff a b) where pure = return; (<*>) = ap
instance Monad (Eff a b) where return = Ret; (>>=) = Bind

execute2 :: Prog -> IO ()
execute2 prog = loop 1 (Ram.init,0,0)
  where
    loop :: Int -> State -> IO ()
    loop i state@(_,pc,_)  =
      if not (continue pc) then pure () else do
        --print (i,pc,op)
        interpret fetch theSemantics state >>= loop (i+1)

    continue :: PC -> Bool
    continue pc = pc >= 0 && pc < progSize

    progSize = PC (length (unProg prog))

    fetch :: PC -> Op
    fetch = \pc -> a ! pc
      where
        a :: Array PC Op
        a = listArray (0,progSize) (unProg prog)

interpret :: (PC -> Op) -> Eff Addr Word8 () -> State -> IO State
interpret fetch eff s0 = loop s0 (\() s -> pure s) eff
  where
    loop :: State -> (x -> State -> IO State) -> Eff Addr Word8 x -> IO State
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
      AddToByte b w -> k (b + w) s
      OffsetAddr a i -> k (a + fromIntegral i) s

----------------------------------------------------------------------
-- compilation effect to residual, and emulate residual

execute3 :: String -> Bool -> Prog -> IO ()
execute3 tag doEmu prog = do
  let
    joinPoints =
      [ pc | (pc,op) <- zip [0.. ] (unProg prog)
           , pc == 0 || op == Lsquare || op == Rsquare
           ]

  let
    compilePC :: PC -> Residual
    compilePC pc = compileAtPC prog joinPoints pc

    codeMap :: CodeMap
    codeMap = CodeMap $ Map.fromList [ (pc, compilePC pc) | pc <- joinPoints ]

  generateFile ("_build/" ++ tag ++ ".c") codeMap

  if doEmu
    then emulate codeMap (PC 0)
    else print "skipping emulation"


generateFile :: Show a => String -> a -> IO ()
generateFile fp a = do
  putStrLn $ "Writing file: " <> fp
  writeFile fp (show a)


compileAtPC :: Prog -> [PC] -> PC -> Residual
compileAtPC prog jumpSet pc0 = do

  let progSize = PC (length (unProg prog))

  let continue :: PC -> Bool
      continue pc = pc >= 0 && pc < progSize

  let fetch :: PC -> Op
      fetch = \pc -> a ! pc
        where
          a :: Array PC Op
          a = listArray (0,progSize) (unProg prog)

  let before :: Int -> CompState -> Residual
      before i cs = do
        compileThen fetch theSemantics cs (after i)

      after :: Int -> CompState -> Residual
      after i cs = do
        let CS{pc} = cs
        if not (continue pc) then R_Halt else
          if pc `elem` jumpSet
          then flushCompState cs
          else before (i+1) cs

  let cs0 :: CompState
      cs0 = CS { nextVar = 0, pc = pc0, mp = 0 }

  before 1 cs0


compileThen
  :: (PC -> Op) -> Eff MP ByteE () -> CompState -> (CompState -> Residual) -> Residual
compileThen fetch eff s0 k0 = loop s0 (\() s -> k0 s) eff
  where
    loop :: CompState -> (x -> CompState -> Residual) -> Eff MP ByteE x -> Residual
    loop s@CS{mp} k = \case
      Ret a -> k a s
      Bind e f -> loop s (\a s -> loop s k (f a))  e
      GetPC -> k (pc s) s
      SetPC pc -> k () s { pc }
      GetMP -> k mp s
      --SetMP mp -> R_SetMP mp (k () s)
      SetMP mp -> k () s { mp }
      GetByte -> freshVar s $ \v s -> R_LetInput v (k (BVar v) s)
      PutByte b -> R_Output b (k () s)
      ReadMem a -> freshVar s $ \v s -> R_LetReadMem v a $ k (BVar v) s
      WriteMem a b -> R_WriteMem a b (k () s)
      Fetch pc -> k (fetch pc) s
      IsZero b -> R_IfZero b (k True s) (k False s)
      AddToByte b w -> k (BAdd b w) s
      OffsetAddr a i -> k (a + fromIntegral i) s


freshVar :: CompState -> (Var -> CompState -> r) -> r
freshVar s@CS{nextVar=v} k = k v (s { nextVar = v + 1 })

data CompState = CS { nextVar :: Var, pc :: PC, mp :: MP } deriving Show

flushCompState :: CompState -> Residual
flushCompState CS{pc,mp} =
  R_SetMP mp (R_Jump pc)


----------------------------------------------------------------------
-- residual code

data CodeMap = CodeMap (Map PC Residual)

data Residual
  = R_Halt
  | R_Jump PC
  | R_IfZero ByteE Residual Residual
  | R_LetInput Var Residual
  | R_LetReadMem Var MP Residual
  | R_WriteMem MP ByteE Residual
  | R_SetMP MP Residual
  | R_Output ByteE Residual

newtype MP = MP { offset :: Int } deriving (Num)

data ByteE
  = BVar Var
  | BAdd ByteE Word8

newtype Var = Var { index :: Int } deriving (Eq,Ord,Num)

----------------------------------------------------------------------
-- display residual code

instance Show CodeMap where
  show (CodeMap m) =
    (unlines . map showLabelledCode . Map.toList) m

showLabelledCode :: (PC,Residual) -> String
showLabelledCode (pc@(PC index),code) =
  show (Label pc) ++ " : { " ++ "trace(" ++ show index ++ "); " ++ show code ++ " }"

newtype Label = Label PC

instance Show Label where
  show (Label (PC n)) = "prog" ++ show n

instance Show Var where
  show Var{index} = "v" ++ show index

instance Show ByteE where
  show = \case
    BVar var -> show var
    BAdd b w -> show b ++ "+" ++ show w

instance Show Residual where
  show = \case
    R_Halt ->
      "exit(0);"
    R_Jump pc ->
      "goto " ++ show (Label pc) ++ ";"
    R_IfZero b r1 r2 ->
      "if (" ++ show b ++ ") { " ++ show r2 ++ " } else { " ++ show r1 ++ " }"
    R_LetInput v r ->
      "byte " ++ show v ++ " = get(); " ++ show r
    R_LetReadMem v mp r ->
      "byte " ++ show v ++ " = *(" ++ show mp ++ "); " ++ show r
    R_WriteMem mp e r ->
      "*(" ++ show mp ++ ") = " ++ show e ++ "; " ++ show r
    R_SetMP mp r ->
      "mp = " ++ show mp ++ "; " ++ show r
    R_Output b r ->
      "put(" ++ show b ++ "); " ++ show r

instance Show MP where
  show MP{offset} = "mp+(" ++ show offset ++ ")"


----------------------------------------------------------------------
-- emulate residual code

emulate :: CodeMap -> PC -> IO ()
emulate (CodeMap m) pc = loop 1 pc rs0
  where
    rs0 = RS { ram = Ram.init, mp = 0 }

    loop :: Int -> PC -> RunState -> IO ()
    loop i pc rs = do
      case Map.lookup pc m of
        Nothing -> error (show ("no residual at pc",pc))
        Just residual -> do
          emulateStep residual rs >>= \case
            Nothing -> pure () -- halt
            Just (pc,rs) -> loop (i+1) pc rs

data RunState = RS { ram :: Ram, mp :: Addr }

type Binds = Map Var Word8

emulateStep :: Residual -> RunState -> IO (Maybe (PC,RunState))
emulateStep r s = loop binds0 s r
  where
    binds0 = Map.empty

    loop :: Binds -> RunState -> Residual -> IO (Maybe (PC,RunState))
    loop m s@RS{mp,ram} = \case
      R_Halt -> pure Nothing
      R_Jump pc -> pure $ Just (pc,s)

      R_IfZero b r1 r2 ->
        loop m s (if (evalB b == 0) then r1 else r2)

      R_LetInput v r -> do
        w <- getW8
        loop (Map.insert v w m) s r

      R_LetReadMem v (MP {offset}) r -> do
        let w = Ram.read ram (mp + fromIntegral offset)
        loop (Map.insert v w m) s r

      R_WriteMem (MP {offset}) e r -> do
        let ram' = Ram.write ram (mp + fromIntegral offset) (evalB e)
        loop m s { ram = ram' } r

      R_SetMP (MP {offset}) r -> do
        loop m s { mp = mp + fromIntegral offset } r

      R_Output b r -> do
        putW8 (evalB b)
        loop m s r

      where
        evalB :: ByteE -> Word8
        evalB = \case
          BVar v -> fromMaybe (error (show ("evalB",v))) $ Map.lookup v m
          BAdd b w -> evalB b + w
