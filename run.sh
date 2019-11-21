#!/bin/bash

ml-build sources.cm Main.main &&
head -1 tiger.grm.desc &&
(
  for tigerSource in testcases/*.tig; do
    echo -- $tigerSource --
    cat $tigerSource
    echo --
    ./tigerc $tigerSource
    echo
  done
)
