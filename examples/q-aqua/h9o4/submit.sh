source ../../../env.sh


i-pi input.xml > log.i-pi &

sleep 10

i-pi-driver -u -h fc0fe8e990be81e66ee918f361efqzeerr -m h9o4 &

wait