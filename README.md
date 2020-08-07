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

