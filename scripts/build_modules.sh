#! /bin/bash

# expects global install of the neato version
GRAPHMOD=graphmod
MODULES=./graphs/modules.dot
MODULESOUT=./graphs/modules.png
SIGNALS=./graphs/signals.dot
SIGNALSOUT=./graphs/signals.png

# build graph
find src test -name '*.purs' | xargs $GRAPHMOD -q \
  -R "Scripts" -d 48,32 > $MODULES \
  || { exit 1; }
# rename graph
sed -i 's/digraph G/digraph Modules/' $MODULES
# redirect nodes - "is imported by"
sed -i 's/\(.*\) -> \(.*\);/\2 -> \1;/' $MODULES
# draw graph to svg
dot -Tpng $MODULES -o $MODULESOUT

# draw signals graph
dot -Tpng $SIGNALS -o $SIGNALSOUT
