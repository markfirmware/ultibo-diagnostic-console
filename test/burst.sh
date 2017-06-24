#/bin/bash

URL=http://163.172.178.31:9980  # Ultima Paris Xenial
URL=http://45.32.223.179:5570   # webstatusprogram Atlanta
URL=http://45.63.115.228:5570   # webstatusprogram Paris
URL=http://104.156.232.107:5570 # webstatusprogram Sydney
URL=http://108.61.117.135:5570  # webstatusprogram Amsterdam
URL=http://163.172.163.75:9980  # Ultima Paris Jessie
URL=http://45.79.200.166:5578   # webstatusprogram markfirmare Atlanta
URL=http://localhost:5588         # webstatusprogram markfirmare Atlanta

EPISODE=$(date +%H%M%S)
CURLTIMEOUT=10
INTERVAL=$1
BURSTCOUNT=$2
COUNTER=0
rm -f errors.txt
clear
echo -n 'starting   ' $BURSTCOUNT ' ... '
while [[ $COUNTER -lt $BURSTCOUNT ]]
do
    FILE=curl.$EPISODE.$COUNTER
    curl -m $CURLTIMEOUT $URL/status > /dev/null 2>> errors.txt &
    sleep $INTERVAL
    let COUNTER=COUNTER+1
done
echo started
time wait
echo -n timeout
grep 'curl: (28)' errors.txt | wc
echo -n refused
grep 'curl: (7)' errors.txt | wc
grep curl errors.txt | grep -v '(28)' | grep -v '(7)'
