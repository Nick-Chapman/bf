
module Ram (Ram,Addr,init,read,write) where

import Prelude hiding (init,read)

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Word8 (Word8)

data Ram = Ram { m :: Map Addr Word8 }

size :: Addr
size = 30000

init :: Ram
init = Ram { m = Map.empty }

read :: Ram -> Addr -> Word8
read Ram{m} a = Map.findWithDefault 0 (a `mod` size) m

write :: Ram -> Addr -> Word8 -> Ram
write Ram{m} a b = Ram { m = Map.insert (a `mod` size) b m }

newtype Addr = Addr Int --{ unAddr :: Int }
  deriving (Eq,Ord,Num,Integral,Real,Enum)
