#! /bin/bash

./scripts/build_all_graphs.sh

# takes forever
spago docs -f html

# move all local docs
find src -name '*.purs' \
  | sed 's/\.purs/.html/' \
  | sed 's/src\///' \
  | sed 's/\//./g' \
  | sed 's/\(.*\)/mv generated-docs\/html\/\1 generated-docs\/\1/' \
  | xargs -L 1 xargs -t

# remove other docs
rm -r generated-docs/html/

# process html, removing modules not in this package
spago run -m Scripts.TrimDocs
