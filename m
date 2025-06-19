#!/bin/bash
cp hello.dck reader
./mix -t -g 16 -x tester >mix.log 2>&1
