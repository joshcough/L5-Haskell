import Control.Applicative
import Data.Traversable
import Prelude hiding (sequence)

data D v a = VD (v a) | App (v a) (v a) deriving (Eq,Ord,Read)

instance (Monad v, Applicative v) => Monad (D v) where
  return = VD . pure
  -- (>>=) :: D v a -> (a -> D v b) -> D v b
  VD  v >>= f = 
    let x = f <$> v
        y = sequence x
    in  _
  --App v args  >>= f = App _ (map _ args)

