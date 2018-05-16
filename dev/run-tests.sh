#!/bin/bash

stack build

geckodriver --port 4444 2>/dev/null >/dev/null &
geckodriver --port 4445 2>/dev/null >/dev/null &
chromedriver --port=9515 &
chromedriver --port=9516 &

stack test --coverage

killall geckodriver
killall chromedriver
