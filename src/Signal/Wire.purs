module Signal.Norn where

import Control.Monad.Eff
import Control.Monad.ST

import Debug.Trace
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

type Order = [Event]

type Config = {graph :: Graph String String, dict :: Map String Event}

type Emitter = {el :: String,
                event :: String,
                config :: Config}

verts :: Graph String String -> [String]
verts (Graph v e) = v

edges :: Graph String String -> [Edge String]
edges (Graph v e) = e

emptyConfig :: Config
emptyConfig = {graph: Graph [] [], dict: empty}

onlyAfter :: String -> Event -> Event
onlyAfter s (Event name trans eff) = Event name trans (\(Tuple st h) -> if member s h
                                                                           then eff (Tuple st h)
                                                                           else return unit)

attachEvent :: Emitter -> [String] -> Event -> Emitter
attachEvent {el=el,event=event,config=config} deps (Event name t e) = {el: el, event: event, config: new_conf}
    where m = config.dict
          new_conf = {graph: (Graph new_verts new_edges), dict: new_map}
          new_map = insert name (Event name t e) m
          new_verts = name : (verts config.graph)
          new_edges = foldlArray (\m i -> (Edge i name):m) (edges $ config.graph) deps

cullMaybeList :: forall a. [Maybe a] -> [a]
cullMaybeList list = foldlArray (\m i -> maybeCons i m) [] list
    where maybeCons (Just i) m = i:m
          maybeCons Nothing m = m

generateExecutionOrder :: Emitter -> [Event]
generateExecutionOrder emitter = final_order
    where config = emitter.config
          name_order = topSort (config.graph)
          dict = config.dict
          final_order = reverse $ cullMaybeList $ Data.Array.map (\name -> lookup name dict) name_order


--foreign import _finalizeEvents """
    --function _finalizeEvents(config, order){
         
    --}
--""" :: forall r. Fn2 Config [Event] (Eff (ne :: NornEff | r) Unit)

--finalizeEvents :: Config -> Eff (ne :: NornEff | r) Unit 
--finalizeEvents (Config _ dict)  = do
    --let order = generateExecutionOrder config
    --runFn2 _finalizeEvents dict order

main = do
    event_a <- noopEvent "foo"
    event_b <- noopEvent "bar"
    event_c <- noopEvent "baz"
    event_d <- noopEvent "foo-bar son"
    let emitter = {el: "document", event: "click", config: emptyConfig}
    {-let c_d = attachEvent emitter ["foo","bar"] event_d emptyConfig-}
    {-let c_a = attachEvent emitter ["bar"] event_a c_d-}
    {-let c_b = attachEvent emitter ["foo"] event_b c_a-}
    {-let c_c = attachEvent emitter [] event_c c_b-}
    let c_a = attachEvent emitter ["bar"] event_a
    let c_b = attachEvent c_a ["baz"] event_b
    let c_c = attachEvent c_b [] event_c
    let c_d = attachEvent c_c ["foo","bar"] event_d
    print $ Data.Array.map (\(Event s _ _) -> s) $ generateExecutionOrder c_d
    print $ scc $ c_d.config.graph
    trace "Foob"
