#!/bin/bash

count=${1:?`echo "No!"`}
re='^[0-9]+$'
if ! [[ $count =~ $re ]] ; then
   echo "error: Not a number" >&2; exit 1
fi

if [[ $count -gt 4 ]] ; then
   echo "Too big (max is 4): ${count}" >&2; exit 1
fi

if [[ $count -lt 1 ]] ; then
   echo "Too små: ${count}" >&2; exit 1
fi

for i in $(seq 1 ${count}); do
echo "Mathematica #${i}"
mathematica >/dev/null 2>&1 & disown
done
