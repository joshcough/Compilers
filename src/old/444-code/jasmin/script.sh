#! /bin/bash
cd ..
modlc Test5.modl > Test5.j
mv Test5.j jasmin
cd jasmin
jasmin Test5.j
java foo