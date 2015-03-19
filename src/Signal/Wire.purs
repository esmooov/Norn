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
import Data.Foldable hiding (lookup)
import Data.Graph

foreign import data NornEff :: !
foreign import data NornState :: *

foreign import emptyNornState """
    function emptyNornState(){
        return {};    
    }
""" :: forall r. Eff (ne :: NornEff | r) NornState

type History = Map String NornState
type Context = Tuple NornState History

data Event = Event String
                   (History -> NornState)
                   (Context -> forall r. Eff (ne :: NornEff | r) Unit)

eventName :: Event -> String
eventName (Event n _ _) = n


noopEvent :: forall r. String -> Eff (ne :: NornEff | r) Event
noopEvent name = do
    state <- emptyNornState
    return $ Event name (\_ -> state) (\_ -> return unit)

data Emitter = Emitter {el :: String,
                        event :: String}
type Order = [Event]

data Cfg  = Cfg {emitter :: Emitter,
                 deps :: [String],
                 event :: Event}
type Config = Graph String Cfg

onlyAfter :: String -> Event -> Event
onlyAfter s (Event name trans eff) = Event name trans (\(Tuple st h) -> if member s h
                                                                           then eff (Tuple st h)
                                                                           else return unit)

attachEvent :: Emitter -> [String] -> Event -> Config -> Config
attachEvent em deps ev (Graph verts edges) = Graph new_verts new_edges
    where vert_name = eventName ev
          new_verts = (Cfg {emitter: em, deps: deps, event: ev}) : verts
          new_edges = foldlArray (\m i -> (Edge i vert_name):m) edges deps


main = do
    trace "Foob"
