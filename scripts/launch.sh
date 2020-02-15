#!/bin/bash
dat=$(date "+%Y-%m-%d_%H:%M:%S")
name=notybot
command=./run.sh
path="/tmp/log";
log_file=./${name}_${dat}.log
config="logfile $log_file
logfile flush 1
log on
logtstamp after 1
logtstamp string \"[ %t: %Y-%m-%d %c:%s ]\012\"
logtstamp on";
echo "$config" > ./log.conf
echo "Logging session in $log_file"
screen -c ./log.conf -dmSL $name $command
