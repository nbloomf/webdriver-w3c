#! /bin/bash

# This script builds and tests webdriver-w3c against stack lts
# resolvers from 13.0 on, that is, resolvers with GHC >=8.6.1. That
# is a hard lower bound because we rely on QuantifiedConstraints.
# Watch out, this takes a long time and should probably only be run
# when preparing a new release. :)

# TODO:
# - when a lts run finishes, write a timestamp and success message to a log

export TASTY_HIDE_SUCCESSES=true

from_maj=${1-9999}
from_min=${2-9999}

# run tests with a specific lts version
test_with_lts_major_minor () {
  echo 
  echo Testing with lts-$1
  echo "Command: stack --resolver lts-$1 test"
  stack --resolver lts-$1 test
  result=$?
  if [ "$result" != "0" ]
  then
    echo "Test failure!"
    echo "Replicate with this command:"
    echo "stack --resolver lts-$1" test
    killall geckodriver
    killall chromedriver
    exit 1
  fi
}

# run tests against all lts resolvers with fixed major version
# skips major versions greater than $1, and if major version
# equals $1, skips minor versions greater than $2.
test_with_lts_major () {
  if [ $from_maj -ge $1 ]
  then
    for (( minor = $2; minor >= 0; minor-- ))
    do
      if [[ ($from_min -ge $minor) || ($from_maj -gt $1) ]]
      then
        test_with_lts_major_minor $1.$minor
      fi
    done
  fi
}

geckodriver --marionette-port 3828 --port 4444 --log error >/dev/null 2>/dev/null &
geckodriver --marionette-port 3829 --port 4445 --log error >/dev/null 2>/dev/null &
chromedriver --port=9515 &
chromedriver --port=9516 &

test_with_lts_major 19 13
test_with_lts_major 18 28
test_with_lts_major 17 15
test_with_lts_major 16 31
test_with_lts_major 15 16
test_with_lts_major 14 26
test_with_lts_major 13 30

killall geckodriver
killall chromedriver

echo "All version tests passed."
