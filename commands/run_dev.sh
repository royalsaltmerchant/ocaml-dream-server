#!/usr/bin/env bash

source_dirs="lib bin"
args=${*:-"server"}
cmd="dune exec ${args}"

function sigint_handler() {
  kill "$(jobs -pr)"
  exit 1
}

trap sigint_handler SIGINT

while true; do
  dune build
  $cmd &
  fswatch -r -1 $source_dirs
  printf "\nRestarting server due to filesystem change\n"
  kill "$(jobs -pr)"
done