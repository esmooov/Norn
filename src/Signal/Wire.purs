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
import Data.Array (reverse)

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
data Config = Config (Graph String String) (Map String Event)

emptyConfig :: Config
emptyConfig = Config (Graph [] []) empty


onlyAfter :: String -> Event -> Event
onlyAfter s (Event name trans eff) = Event name trans (\(Tuple st h) -> if member s h
                                                                           then eff (Tuple st h)
                                                                           else return unit)

attachEvent :: Emitter -> [String] -> Event -> Config -> Config
attachEvent em deps (Event name t e) (Config (Graph verts edges) m) = Config (Graph new_verts new_edges) new_map
    where new_map = insert name (Event name t e) m
          new_verts = name : verts
          new_edges = foldlArray (\m i -> (Edge i name):m) edges deps

cullMaybeList :: forall a. [Maybe a] -> [a]
cullMaybeList list = foldlArray (\m i -> maybeCons i m) [] list
    where maybeCons (Just i) m = i:m
          maybeCons Nothing m = m

generateExecutionOrder :: Config -> [Event]
generateExecutionOrder (Config graph dict) = final_order
    where name_order = topSort graph
          final_order = reverse $ cullMaybeList $ Data.Array.map (\name -> lookup name dict) name_order

main = do
    event_a <- noopEvent "foo"
    event_b <- noopEvent "bar"
    event_c <- noopEvent "baz"
    event_d <- noopEvent "foo-bar son"
    let emitter = Emitter {el: "document", event: "click"}
    let c_d = attachEvent emitter ["foo","bar"] event_d emptyConfig
    let c_a = attachEvent emitter ["bar"] event_a c_d
    let c_b = attachEvent emitter ["foo"] event_b c_a
    let c_c = attachEvent emitter [] event_c c_b
    print $ Data.Array.map (\(Event s _ _) -> s) $ generateExecutionOrder c_c
    trace "Foob"
