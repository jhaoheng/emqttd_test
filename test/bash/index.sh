#!/bin/bash

broker=''
sub_topic='/test/#'
pub_topic='/test/123'
msg='hello'
user=''
pw=''

while [[ 1 ]]; do

    echo "\n\r****" >> log.txt
    echo $( date "+%D %T") "start connect " >> log.txt
    mosquitto_sub -h $broker -t $sub_topic -u $user -P $pw | xargs -L1 sh -c 'date "+%D %T GET $0 $*"' >> log.txt &
    ret=$?

    if [[ 0 -ne $ret ]]; then
        echo "connect error" >> log.txt
        else
        echo "success connect" >> log.txt
        pid="$!"
        # echo $pid
        sleep 2s

        mosquitto_pub -h $broker -t $pub_topic -m $msg -u $user -P $pw | echo $( date "+%D %T") "pub hello"  >> log.txt

        sleep 5s

        killall mosquitto_sub

    fi

    echo "****\n\r" >> log.txt

done