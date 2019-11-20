#!/bin/bash

ml-build sources.cm Main.main &&
(
  for tigerSource in testcases/*.tig; do
    echo -- $tigerSource --
    cat $tigerSource
    echo --
    sml @SMLload=sources.x86-linux $tigerSource
    echo
  done
) > run.out
