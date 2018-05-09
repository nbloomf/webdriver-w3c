#!/bin/bash

stack build

geckodriver 2>/dev/null >/dev/null &
chromedriver &

stack test --coverage

killall geckodriver
killall chromedriver
