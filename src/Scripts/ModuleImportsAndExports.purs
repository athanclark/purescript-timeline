module Scripts.ModuleImportsAndExports where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Data.Array (snoc, init, sortWith) as Array
import Data.Array.NonEmpty ((!!))
import Data.Foldable (intercalate)
import Data.Traversable (traverse_)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.String.Regex (Regex, regex, test, match)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (lines)
import Data.Map (Map)
import Data.Map (insert, empty, lookup, toUnfoldable, insertWith) as Map
import Effect (Effect)
import Effect.Ref (new, read, write) as Ref
import Effect.Exception (throw)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Node.Path (FilePath)
import Node.FS.Aff (readTextFile)
import Node.Encoding (Encoding(UTF8))

file :: FilePath
file = "graphs/modules.dot"

graphNodeRegex :: Regex
graphNodeRegex =
  unsafePartial
    $ case regex "^(u[0-9]+)\\[label=\"([a-zA-Z]+)\"" noFlags of
        Right r -> r

subgraphRegex :: Regex
subgraphRegex =
  unsafePartial
    $ case regex "^subgraph" noFlags of
        Right r -> r

subgraphLabelRegex :: Regex
subgraphLabelRegex =
  unsafePartial
    $ case regex "^label=\"([a-zA-Z]+)\"" noFlags of
        Right r -> r

closeSubgraphRegex :: Regex
closeSubgraphRegex =
  unsafePartial
    $ case regex "^}$" noFlags of
        Right r -> r

isNewSubgraph :: String -> Boolean
isNewSubgraph = test subgraphRegex

isClosingSubgraph :: String -> Boolean
isClosingSubgraph = test closeSubgraphRegex

getSubgraphLabel :: String -> Maybe String
getSubgraphLabel s = case match subgraphLabelRegex s of
  Nothing -> Nothing
  Just a -> join (a !! 1)

getGraphNode :: String -> Maybe { node :: String, label :: String }
getGraphNode s = case match graphNodeRegex s of
  Nothing -> Nothing
  Just a -> do
    node <- join (a !! 1)
    label <- join (a !! 2)
    pure { node, label }

arrowRegex :: Regex
arrowRegex =
  unsafePartial
    $ case regex "^(u[0-9]+) -> (u[0-9]+);$" noFlags of
        Right r -> r

getArrow :: String -> Maybe { from :: String, to :: String }
getArrow line = case match arrowRegex line of
  Nothing -> Nothing
  Just a -> do
    from <- join (a !! 1)
    to <- join (a !! 2)
    pure { from, to }

type Accumulator
  = { mapping :: Map String (Array String)
    , prefix :: Array String
    , nextIsSubgraphLabel :: Boolean
    }

showModule :: Array String -> String
showModule = intercalate "."

getNodes ::
  Array String ->
  Effect
    { labels :: Map String (Array String)
    , spread :: Map String Int
    , hoarding :: Map String Int
    }
getNodes xs = do
  accRef <-
    Ref.new
      { mapping: Map.empty
      , prefix: []
      , nextIsSubgraphLabel: false
      }
  spreadRef <- Ref.new Map.empty
  hoardingRef <- Ref.new Map.empty
  let
    write :: Accumulator -> Effect Unit
    write x = Ref.write x accRef

    go :: Int -> String -> Effect Unit
    go index line = do
      { mapping, prefix, nextIsSubgraphLabel } <- Ref.read accRef
      case unit of
        _
          | isNewSubgraph line ->
            write
              { mapping
              , prefix
              , nextIsSubgraphLabel: true
              }
          | nextIsSubgraphLabel -> case getSubgraphLabel line of
            Nothing -> throw $ "Can't get subgraph label when expecting it: " <> show line <> ", line " <> show index
            Just label ->
              write
                { mapping
                , prefix: Array.snoc prefix label
                , nextIsSubgraphLabel: false
                }
          | isClosingSubgraph line -> case Array.init prefix of
            Nothing -> pure unit -- throw $ "Can't close subgraph - line: " <> show index
            Just prefix' ->
              write
                { mapping
                , prefix: prefix'
                , nextIsSubgraphLabel: false
                }
          | otherwise -> case getGraphNode line of
            Nothing -> case getArrow line of
              Nothing -> pure unit
              Just { from, to } -> do
                spread <- Ref.read spreadRef
                Ref.write (Map.insertWith (+) from 1 spread) spreadRef
                hoarding <- Ref.read hoardingRef
                Ref.write (Map.insertWith (+) to 1 hoarding) hoardingRef
            Just { node, label } ->
              write
                { mapping: Map.insert node (Array.snoc prefix label) mapping
                , prefix
                , nextIsSubgraphLabel: false
                }
  void (traverseWithIndex go xs)
  { mapping, prefix } <- Ref.read accRef
  if prefix /= [] then
    throw "Error! Prefix not empty!"
  else do
    spread <- Ref.read spreadRef
    hoarding <- Ref.read hoardingRef
    pure
      { labels: mapping
      , spread
      , hoarding
      }

main :: Effect Unit
main =
  launchAff_ do
    text <- readTextFile UTF8 file
    let
      xs = lines text
    liftEffect do
      { labels, spread, hoarding } <- getNodes xs
      let
        getLabel :: String -> String
        getLabel l =
          unsafePartial
            $ case Map.lookup l labels of
                Just x -> showModule x

        flipLabel (Tuple u c) = Tuple c (getLabel u)

        sortCount = Array.sortWith (\(Tuple k _) -> k)

        spread' :: Array _
        spread' = Map.toUnfoldable spread

        spread'' = sortCount (map flipLabel spread')

        hoarding' :: Array _
        hoarding' = Map.toUnfoldable hoarding

        hoarding'' = sortCount (map flipLabel hoarding')
      let
        go use (Tuple k v) = log $ "module: " <> v <> ", " <> use <> ": " <> show k
      log ""
      log "Most Imported Modules: "
      log ""
      traverse_ (go "used by") spread''
      log ""
      log "Most Importing Modules: "
      log ""
      traverse_ (go "using") hoarding''
