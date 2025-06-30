#!/bin/bash

for ((i = 1 ; i < 10000 ; i++ )); do 
    $1 -n $i;
    exit_code=$?
    echo "$i -> $exit_code"; 
    if [[ $exit_code != 0 ]]; then
        break;
    fi
done

