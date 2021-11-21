#!/bin/bash

let number=$RANDOM
let number2=$RANDOM

if [ $number -gt 25000 ]; then
    echo "What is your favorite topic?"
else
    echo "What is your favorite movie?"
fi

read $REPLY

if [ $number2 -gt 25000 ]; then
    echo "What is your favorite topic?"
else
    echo "What is your favorite movie?"
fi

read $REPLY
