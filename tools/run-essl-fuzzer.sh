#!/bin/sh

cd `dirname $0`

COUNTER=0
while [ true ]
do
	./essl-fuzzer.py -s $COUNTER | ./cgc >/dev/null || {
		echo "failed at seed $COUNTER"
	}
	COUNTER=$((COUNTER + 1))
done
