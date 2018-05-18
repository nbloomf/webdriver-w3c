#!/bin/bash

stack build
stack install

geckodriver --port 4444 2>/dev/null >/dev/null &
geckodriver --port 4445 2>/dev/null >/dev/null &
chromedriver --port=9515 &
chromedriver --port=9516 &

wd-tasty-demo --wd-remote-ends 'geckodriver: https://localhost:4444'
wd-tasty-demo --wd-remote-ends 'geckodriver: https://localhost:4444' --wd-verbosity silent
wd-tasty-demo --wd-driver chromedriver --wd-response-format chromedriver --wd-remote-ends 'chromedriver: https://localhost:9515'
wd-tasty-demo --wd-remote-ends 'geckodriver: https://localhost:4444' --num-threads 2

killall geckodriver
killall chromedriver
