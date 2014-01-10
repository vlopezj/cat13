#!/bin/bash


rm -R victor
mkdir -p victor

for a in chap*; do
    cp $a/victor.pdf victor/victor-$a.pdf
done

cp chap06/ex5.hs/ex5.hs victor/victor-chap06-ex5.hs
