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
type Context = Tuple NornState History

data Event = Event String
                   (History -> NornState)
                   (Context -> forall r. Eff (ne :: NornEff | r) Unit)

data Emitter = Emitter {el :: String,
                        event :: String}
type Order = [Event]

data Cfg  = Cfg {emitter :: Emitter,
                 deps :: [String],
                 event :: Event}
type Config = [Cfg]

onlyAfter :: String -> Event -> Event
onlyAfter s (Event name trans eff) = Event name trans (\(Tuple st h) -> if member s h
                                                                           then eff (Tuple st h)
                                                                           else return unit)

attachEvent :: Emitter -> [String] -> Event -> Config -> Config
attachEvent em deps ev c = (Cfg {emitter: em, deps: deps, event: ev}) : c


main = do
    trace "Foob"
