#!/bin/bash

stack build
stack install

geckodriver --marionette-port 4454 --port 4444 --log error >/dev/null 2>/dev/null &
geckodriver --marionette-port 4455 --port 4445 --log error >/dev/null 2>/dev/null &
chromedriver --port=9515 &
chromedriver --port=9516 &

wd-tasty-demo --wd-remote-ends 'geckodriver https://localhost:4444'
wd-tasty-demo --wd-remote-ends 'geckodriver https://localhost:4444' --wd-verbosity silent
wd-tasty-demo --wd-driver chromedriver --wd-remote-ends 'chromedriver https://localhost:9515'
wd-tasty-demo --wd-remote-ends 'geckodriver https://localhost:4444' --num-threads 2

killall geckodriver
killall chromedriver
