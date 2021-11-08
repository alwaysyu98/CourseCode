#!/usr/bin/env bash

count=0

while true; do
  count=$(($count+1))
  ./ex3-task.sh > stdout 2> stderr
  if [[ $? -ne 0 ]]; then
    echo "stdout: $(cat stdout)"
    echo "stderr: $(cat stderr)"
    echo "count: $count"
    exit 0
  fi
done
