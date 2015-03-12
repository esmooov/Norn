module Signal.Norn where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.ST

import Debug.Trace
import Data.Either
import Data.Tuple
import Data.Map
import Data.Maybe
import Data.Function
import Data.Profunctor

foreign import data NornEff :: !
foreign import data NornState :: *

type History = Map String NornState

data Event = Event (History -> NornState)
                   (Tuple NornState History -> forall r. Eff (ne :: NornEff | r) Unit)

onlyAfter :: String -> Event -> Event
onlyAfter s (Event trans eff) = Event trans (\(Tuple st h) -> if member s h
                                                                   then eff (Tuple st h)
                                                                   else return unit)



main = do
    trace "Foob"
