#!/bin/bash

#Get mathematica process numbers
this=( $(ps -ef|grep ".*bin/sh.*mathematica.*" | grep -v "grep" | awk '{ print $2 }') )

count=0
for ppid in ${this[@]}; do
echo "Mathematica #${count}: pid ${ppid}"
taskset -p ${ppid}
taskset -cp ${count} ${ppid}
count=$((count + 1))
done
