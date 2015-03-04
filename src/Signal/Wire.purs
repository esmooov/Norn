module Signal.Wire where

import Control.Monad.Eff
import Control.Monad.ST

import Debug.Trace
import Data.Either
import Data.Tuple
import Data.Function

foreign import data Action :: !
type UpdatePool a e = forall s. [STRef a (Tuple (Eff (st :: ST a | e) Unit) (Eff (st :: ST a | e) s))]
data SignalGen a e s = SG {unSG :: (STRef a (UpdatePool a e) -> Eff e s)}
data Signal a e s = S (Eff (st :: ST a | e) s)

data Phase a = Ready a | Updated a a

foreign import throw """
    function throw(e){
        console.log("Error: ",e);  
    }
""" :: forall r. String -> Eff (e :: Action | r) Unit

addSignal :: forall s a e r. (s -> Eff (st :: ST a | e) s) -> 
                             (s -> Eff (st :: ST a | e) Unit) -> 
                             STRef a (Phase s) -> 
                             STRef a (UpdatePool a e) -> 
                             Eff (st :: ST a | e) (Signal a e s)
addSignal sample update ref pool = do
    let upd = readSTRef ref >>= \v -> case v of
                Ready x -> update x
                _       -> return unit
        fin = readSTRef ref >>= \v -> case v of
                Updated x _ -> writeSTRef ref (Ready x)
                Ready x -> writeSTRef ref (Ready x)
                --_         -> throw "Signal Not Updated"
        sig = S $ readSTRef ref >>= \v -> case v of
                Ready x -> sample x
                Updated _ x -> return x
    --updateActions <- mkWeak sig (upd,fin) Nothing
    t <- newSTRef $ Tuple upd fin
    modifySTRef pool (\p -> t:p)
    --return sig
    return sig


--delay :: forall a. a -> Signal a -> SignalGen (Signal a)
--delay x0 (S s) = SG $ \pool -> do
    --ref <- newSTRef (Ready x0)
    --let update x = s >>= \x' -> writeSTRef ref (Updated x' x)
    --addSignal return update ref pool

--start :: forall a r. SignalGen (Signal a) -> Eff (a :: (a :: Action | r) a)
--start (SG gen) = do
--foreign import data CurFun :: * -> !

--foreign import add """
    --function add(a){
        --return function(b){console.log(a + b);}  
    --};
--""" :: forall r. Number -> (Eff (curfun :: CurFun Number | r) SignalGen) 



main = do
    trace "Foob"
