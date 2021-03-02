#!/bin/bash
set -e
usage() {
  echo "Ensure directory exists and git mv to a prefix"
  echo ""
  echo "usage:"
  echo "  git-move-to.sh [options] <prefix> <file>"
  echo ""
  echo "[options]"
  echo "  --dry-run: print commands but don't run"
  echo ""
  echo "example 1: Move one file to attic/"
  echo "  git-move-to.sh attic biolink/foo"
  echo ""
  echo "  expands to:"
  echo "    mkdir -p attic/biolink; git mv biolink/foo attic/biolink/foo"
  echo ""
  echo "example 2: Test moving a list of files."
  echo "  cat | xargs -L1 bash renames/git-move-to.sh --dry-run attic"
  echo "    biolink/example-dhx30-rtx.rkt"
  echo "    biolink/make-kg-edge-general.rkt"
  echo "    <Ctrl+D>"
}

dryRun=0
if [[ "$1" == "--dry-run" ]]
then
  dryRun=1
  shift
fi
reld="$1"
relf="$2"
if [[ "$reld" == "" ]]
then
  usage
elif [[ "$relf" == "" ]]
then
  usage
else
  relfDest="${reld}/${relf}"
  reldDest=$(dirname "$relfDest")
  if [[ "$dryRun" -ne 0 ]]
  then
    echo mkdir -p "$reldDest"
    echo git mv "$relf" "$reldDest"/
  else
    mkdir -p "$reldDest"
    git mv "$relf" "$reldDest"/
  fi
fi
