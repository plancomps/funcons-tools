#!/bin/bash

run_test() {
  out=$(cabal run runfct -- --config $1);
  if [[ $out ]]
    then  echo $1:;
          echo "$out";
  fi
}
export -f run_test
find $1 -name "*.config" | xargs -n1 -I{} bash -c 'run_test "$@"' _ {} 
