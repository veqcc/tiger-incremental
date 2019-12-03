#!/bin/bash

ml-build sources.cm Main.main &&
head -1 tiger.grm.desc &&
(
  for tigerSource in testcases/*.tig; do
    echo -- $tigerSource --
    cat $tigerSource
    ./tigerc $tigerSource
    gcc -o tmp tmp.s
    ./tmp
    echo "=>" $? "\n"
  done
)
