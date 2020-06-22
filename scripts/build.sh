#!/bin/bash


# ----------- compiling

spago build \
     || { echo "Build Failed"; exit 1; }

echo "Built"

# ----------- update module graph

./scripts/build_modules.sh \
     || { echo "Module Graph Failed"; exit 1; }
echo "Module Graph Built"

echo "Finished"
