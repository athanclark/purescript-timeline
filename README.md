# purescript-timeline

[![Build Status](https://travis-ci.org/openchronology/purescript-timeline.svg?branch=master)](https://travis-ci.org/openchronology/purescript-timeline)

This project defines the types and operations used in a timeline. Although somewhat integrated,
it tries to separate the operational aspects of the types from their user interface, and also
provides encoding implementations for the types.

## Definitions

There are a number of data types using obscure verbiage in this project:

- A "**TimeSpace**" is a physically traversable, presented view of multiple timelines, over some **TimeScale**.
- A "**TimeScale**" is the definition for how time passes over some units of space (or vise-versa); for instance,
  linearly or logarithmically, and in-between some values. Furthermore, the definition for what _"time"_ actually means
  in this context is defined here: _"Am I considering any current-era date, or just the year? Do I consider day time a factor?
  Am I concerned with century values, or do I want to think in terms of 'Millions of Years Ago'?"_
- A "**Timeline**" is a grouping of **Events** and **TimeSpans** over a **TimeSpace**.
- An "**Event**" is a document that identifies a specific time defined in a **TimeScale**, which is associated with one or more **TimeLines**.
- A "**TimeSpan**" serves two purposes: a document that identifies a range between two specific times defined in a **TimeScale**, _and_ a
  potential sub-**TimeSpace**, where a new **TimeScale** and series of **TimeLines** may be defined; for instance, the parent **Timeline** may define
  Earth's history logarithmically, and a small **TimeSpan** may define Human history with a linear **TimeSpace**.
- A "**TimeIndex**" comes in three flavors:
  - "**Human**" - an Index that makes sense to humans; for instance, a Current-Era date "March 27th, 2020".
  - "**Presentable**" - an Index that makes sense to a **TimeSpace** - which can only be a JavaScript `Number`, to denote some offset in SVG
  - "**Comparable**" - an Index that makes sense to a _total order_ - probably also a JavaScript `Number`; we'll use this to sort, move, and order the data internally
  - "**Storable**" - an Index that makes sense to JSON and ArrayBuffers - minimalistic approach to representing the **Human TimeIndex** without losing any data.
    This could be a `Int8` for small enough **TimeScales**, or it may just be a `UTF-8 String` for a Date-Time.

## Data Hierarchy

- TimeSpace
  - TimeScale
    - TimeIndex
  - TimeLine
    - Event
      - TimeIndex
    - TimeSpan
      - TimeIndex
      - TimeSpace


# Signals

This package defines many different inter-woven signals, each behaving as a "source of truth" for some smaller component
that views it. However, we have to keep all of these sources of truth _correct_. The following diagram describes the
current network of data flow:

![](https://github.com/openchronology/purescript-timeline/raw/master/graphs/signals.png)

## Mappings

The recursive data type that describes the stored document isn't sufficient for incremental modification; for that, we'll
need to make a series of mappings which _reference_ each other - following the references can recreate the recursion.

There are four primary mappings: `TimeSpaces`, `Timelines`, `Events`, and `TimeSpans` - these are sufficient for storing
everything in both an OpenChronology document, and its user interface.

However, changes in each of these must be reflected (when necessary) to their constituent children or parents. For instance,
what if one child is removed? Should it's parent reference be eliminated? What about re-sorting of children, should they
care? What if a child decides it wants to get adopted - how do we let the parent know?

- `5` is an edge that represents the binding between `TimeSpaces` and `Timelines`
- `6` is two edges that represents the binding between `TimeSpaces` and both `Events` and `TimeSpans` - as "siblings"
- `7` is two edges that represents the binding between `Timelines` and both `Events` and `TimeSpans` - as "children"

## Views

When using the user interface, you're only viewing _one_ `TimeSpace` at a time. However, this is exactly how the
document gets _edited_ - incrementally. Therefore, we need to address how we present the relative data, and how
its modification should affect both the _rest_ of the viewed data, and the stored data not currently in view.

There are four primary views, and two secondary ones: `TimeSpaceViewed`, `TimelinesViewed`, `SiblingsViewed`, and `ChildrenViewed`
are all primary components of the user interface, while `Selected TimeSpace Index` explicitly states the
index (as a series of parents) of the currently viewed timespace, and `Selected Timeline` explicitly states which
timeline is currently being selected from the listing of `TimelinesViewed`.

- `1` is an edge that represents how changes to the viewed timespace can reflect changes to itself in the mapping, and vise-versa.
- `2` is two edges that represent how changes to the listing of timelines (belonging to the current timespace) can reflect
  changes to their stored versions, _and_ how they're listed in the currently viewed timespace (and vise-versa).
- `3` is three edges that represent how changes to the listing of siblings (belonging to the current timespace) can reflect
  changes to their stored events or timespans, _and_ how they're listed in the currently viewed timespace (and vise-versa).
- `4` is three edges that represent how changes to the listing of children (belonging to the current timeline) can reflect
  changes to their stored events or timespans, _and_ how they're listed in the currently selected timeline (and vise-versa).

## Composition

You'll notice the following commutive properties, in terms of edge tracing:

- `1 . 2 == 5 . 2` - timelines viewed updates timespace viewed, which updates itself == timelines viewed updates itself, which the
  mapping equalizes
- `1 . 3 == 6 . 3` - siblings viewed updates timespace viewed, which updates itself == siblings viewed updates itself, which the
  mapping equalizes
- `4 == 7 . 4` - children viewed updates itself == children viewed updates timelines viewed, which the mapping equalizes
