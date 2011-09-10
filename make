#!/bin/bash

for input_file in $(find . -iname "*.h.hs" -print)
do
  echo "runhaskell $input_file > ${input_file%.*}"
  runhaskell $input_file > ${input_file%.*}
done

g++ -Wall -Wextra -Werror -lcurses -o abyss $(find . -iname "*.cpp" -print)
