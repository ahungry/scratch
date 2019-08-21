#!/bin/sh

# echo
# echo
# echo "Allman"
# echo
# clang-format -style='{BreakBeforeBraces: Allman}' ./sample.c | sed -e 's/[^ ]/X/g'

# echo
# echo
# echo "GNU"
# echo
# clang-format -style='{BreakBeforeBraces: GNU}' ./sample.c | sed -e 's/[^ ]/X/g'

echo
echo
echo "Linux"
echo
clang-format -style='{BreakBeforeBraces: Linux}' ./sample.c | sed -e 's/[^ ]/X/g'

# echo
# echo
# echo "Stroustrup"
# echo
# clang-format -style='{BreakBeforeBraces: Stroustrup}' ./sample.c | sed -e 's/[^ ]/X/g'

# echo
# echo
# echo "Mozilla"
# echo
# clang-format -style='{BreakBeforeBraces: Mozilla}' ./sample.c | sed -e 's/[^ ]/X/g'

# echo
# echo
# echo "WebKit"
# echo
# clang-format -style='{BreakBeforeBraces: WebKit}' ./sample.c | sed -e 's/[^ ]/X/g'

echo
echo
echo "GNU"
echo
clang-format -style='gnu' ./sample.c | sed -e 's/[^ ]/X/g'
