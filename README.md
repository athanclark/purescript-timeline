# purescript-timeline

There are a number of data types using obscure verbiage in this project - roughly speaking, here is a definition list:

- A "**TimeSpace**" is a physically traversable, presented view of multiple timelines, over some _TimeScale_.
- A "**TimeScale**" is the definition for how time passes over some units of space (or vise-versa); for instance,
  linearly or logarithmically, and in-between some values. Furthermore, the definition for what _"time"_ actually means
  in this context is defined here: _"Am I considering any current-era date, or just the year? Do I consider day time a factor?
  Am I concerned with century values, or do I want to think in terms of 'Millions of Years Ago'?"_
- A "**Timeline**" is a grouping of _Events_ and _TimeSpans_ over a _TimeSpace_.
- An "**Event**" is a document that identifies a specific time defined in a _TimeScale_, which is associated with one or more _TimeLines_.
- A "**TimeSpan**" serves two purposes: a document that identifies a range between two specific times defined in a _TimeScale_, _and_ a
  potential sub-_TimeSpace_, where a new _TimeScale_ and series of _TimeLines_ may be defined; for instance, the parent _Timeline_ may define
  Earth's history logarithmically, and a small _TimeSpan_ may define Human history with a linear _TimeSpace_.
