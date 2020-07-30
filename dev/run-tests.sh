#!/bin/bash

stack build

geckodriver --marionette-port 3828 --port 4444 --log error >/dev/null 2>/dev/null &
geckodriver --marionette-port 3829 --port 4445 --log error >/dev/null 2>/dev/null &
chromedriver --port=9515 &
chromedriver --port=9516 &

stack test --coverage
stack hpc report .

killall geckodriver
killall chromedriver
