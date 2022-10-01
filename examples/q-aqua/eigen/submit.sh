source ../../../env.sh 

i-pi input.xml > log.i-pi &

sleep 10

i-pi-driver -u -h eigen000 -m eigen &

wait