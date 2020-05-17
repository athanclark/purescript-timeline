#! /bin/bash

# build graph
find src -name '*.purs' | xargs graphmod -q -R 'MaterialUI' -d 12,8 > modules.dot
# rename graph
sed -i 's/digraph G/digraph Modules/' modules.dot
# redirect nodes - "is imported by"
sed -i 's/\(.*\) -> \(.*\);/\2 -> \1;/' modules.dot
# draw graph to svg
dot -Tsvg modules.dot -o modules.svg
