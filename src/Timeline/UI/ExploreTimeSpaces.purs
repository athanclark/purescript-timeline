module Timeline.UI.ExploreTimeSpaces where

import Timeline.Time.Bounds (DecidedBounds(..))
import Prelude
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Array (uncons, unsnoc, snoc, foldl, foldr) as Array
import Data.IxSet.Demi (Index, IxDemiSet)
import Data.IxSet.Demi (intoFrom, size, updateValue, lookup, fromFoldable, empty) as IxDemiSet
import Effect (Effect)
import Zeta.Types (READ, WRITE) as S
import IxZeta (IxSignal, make) as IxSig
import Partial.Unsafe (unsafePartial)

-- | Used to assist the recursive type, due to children being a mapping
newtype WithBounds a
  = WithBounds
  { bounds :: DecidedBounds
  , timeSpaces :: a
  }

instance functorWithBounds :: Functor WithBounds where
  map f (WithBounds x) = WithBounds x { timeSpaces = f x.timeSpaces }

instance applyWithBounds :: Apply WithBounds where
  apply (WithBounds f) (WithBounds x) = WithBounds x { timeSpaces = f.timeSpaces x.timeSpaces }

-- | Should be treated like a global state - is what's communicated to the dialog via signal
newtype ExploreTimeSpaces
  = ExploreTimeSpaces
  { name :: String
  , children :: IxDemiSet DecidedBounds ExploreTimeSpaces
  }

-- | Should be treated as only used by the dialog in it's component state
newtype ExploreTimeSpacesWithAux
  = ExploreTimeSpacesWithAux
  { name :: String
  , children ::
      Maybe
        { open :: Boolean
        , children :: IxDemiSet DecidedBounds ExploreTimeSpacesWithAux
        }
  }

newExploreTimeSpacesSignal :: Effect (IxSig.IxSignal ( read :: S.READ, write :: S.WRITE ) (WithBounds ExploreTimeSpaces))
newExploreTimeSpacesSignal =
  IxSig.make
    $ WithBounds
        { bounds:
            DecidedBoundsNumber
              { begin: 1234.0
              , end: 5678.0
              }
        , timeSpaces:
            ExploreTimeSpaces
              { name: "TimeSpace Name"
              , children:
                  let
                    { set } =
                      IxDemiSet.fromFoldable
                        [ Tuple (DecidedBoundsNumber { begin: 0.0, end: 1.0 })
                            $ ExploreTimeSpaces
                                { name: "TimeSpace Child 1", children: IxDemiSet.empty }
                        , Tuple (DecidedBoundsNumber { begin: 1.0, end: 2.0 })
                            $ ExploreTimeSpaces
                                { name: "TimeSpace Child 2"
                                , children:
                                    let
                                      { set } =
                                        IxDemiSet.fromFoldable
                                          [ Tuple (DecidedBoundsNumber { begin: 2.0, end: 3.0 })
                                              $ ExploreTimeSpaces
                                                  { name: "TimeSpace GrandChild 1", children: IxDemiSet.empty }
                                          , Tuple (DecidedBoundsNumber { begin: 4.0, end: 5.0 })
                                              $ ExploreTimeSpaces
                                                  { name: "TimeSpace GrandChild 2", children: IxDemiSet.empty }
                                          ]
                                    in
                                      set
                                }
                        , Tuple (DecidedBoundsNumber { begin: 6.0, end: 7.0 })
                            $ ExploreTimeSpaces
                                { name: "TimeSpace Child 3", children: IxDemiSet.empty }
                        ]
                  in
                    set
              }
        }

-- | Initial state for explore time spaces
exploreTimeSpacesWithAux :: ExploreTimeSpaces -> ExploreTimeSpacesWithAux
exploreTimeSpacesWithAux (ExploreTimeSpaces { name, children }) =
  ExploreTimeSpacesWithAux
    { name
    , children:
        if IxDemiSet.size children == 0 then
          Nothing
        else
          Just
            { open: false
            , children: map exploreTimeSpacesWithAux children
            }
    }

updateExploreTimeSpacesWithAux :: ExploreTimeSpacesWithAux -> ExploreTimeSpaces -> ExploreTimeSpacesWithAux
updateExploreTimeSpacesWithAux (ExploreTimeSpacesWithAux x) (ExploreTimeSpaces y) =
  ExploreTimeSpacesWithAux
    x
      { name = y.name
      , children =
        if IxDemiSet.size y.children == 0 then
          Nothing
        else
          Just
            $ case x.children of
                Nothing ->
                  { open: false
                  , children: map exploreTimeSpacesWithAux y.children
                  }
                Just { open, children: xchildren } ->
                  { open
                  , children:
                      IxDemiSet.intoFrom updateExploreTimeSpacesWithAux exploreTimeSpacesWithAux xchildren y.children
                  }
      }

toggleOpen :: Array Index -> ExploreTimeSpacesWithAux -> ExploreTimeSpacesWithAux
toggleOpen index orig@(ExploreTimeSpacesWithAux x) = case x.children of
  Nothing -> orig
  Just y -> case Array.uncons index of
    Nothing -> ExploreTimeSpacesWithAux x { children = Just y { open = not y.open } }
    Just { head, tail } -> case IxDemiSet.lookup head y.children of
      Nothing -> orig
      Just { value } ->
        ExploreTimeSpacesWithAux
          x
            { children =
              Just
                y
                  { children = IxDemiSet.updateValue head (toggleOpen tail value) y.children
                  }
            }

setOpen :: Boolean -> Array Index -> ExploreTimeSpacesWithAux -> ExploreTimeSpacesWithAux
setOpen open index orig@(ExploreTimeSpacesWithAux x) = case x.children of
  Nothing -> orig
  Just y -> case Array.uncons index of
    Nothing -> ExploreTimeSpacesWithAux x { children = Just y { open = open } }
    Just { head, tail } -> case IxDemiSet.lookup head y.children of
      Nothing -> orig
      Just { value } ->
        ExploreTimeSpacesWithAux
          x
            { children =
              Just
                y
                  { children = IxDemiSet.updateValue head (setOpen open tail value) y.children
                  }
            }

openThrough :: Array Index -> ExploreTimeSpacesWithAux -> ExploreTimeSpacesWithAux
openThrough is ts =
  let
    iss :: Array (Array Index)
    iss =
      let
        basis = [ [] ]

        go :: Array (Array Index) -> Index -> _
        go acc i =
          unsafePartial
            $ case Array.unsnoc acc of
                Just { init, last } -> Array.snoc acc (Array.snoc last i)
      in
        Array.foldl go basis is
  in
    Array.foldr (setOpen true) ts iss
