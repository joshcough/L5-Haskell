#!/bin/bash

for f in $1/*.$2; do
  echo ========== file: $f ==========
  echo --- interpreter results ---
  linterp $f
  echo --- native results ---
  lc -x $f
  echo
  echo
done
