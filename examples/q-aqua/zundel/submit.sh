source ../../../env.sh

i-pi input.xml > log.i-pi &

sleep 20

i-pi-driver -u -h fc0fe8e990be81e66ee918f361efqzzzrr444 -m zundel &

wait