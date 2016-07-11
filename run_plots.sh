#!/bin/bash

for i in {1..6}
  do
    /usr/local/bin/Rscript plot${i}.R
  done

