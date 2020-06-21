#! /bin/bash

echo ""
echo ""
echo "Number of modules:"
find src/ -name "*.purs" | wc --lines

echo ""
echo ""
echo "Lines of Code per module:"
echo ""
echo ""
find src/ -name "*.purs" | xargs wc --lines | sort -nr

echo ""
spago run -m Scripts.ModuleImportsAndExports
