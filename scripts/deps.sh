#!/bin/bash

echo "[*] Installing dependencies.."

#set -e


echo "[1] Haskell Tool Stack:"

stack_path=$(which stack)

if [ -z $stack_path ]; then
    echo "Installing..."
    wget -qO- https://get.haskellstack.org/ | sh
else
    echo "Already installed in $stack_path"
fi


echo "[2] Mongo Database:"

mongod_path=$(which mongod)
aarch=$(uname -m)
distribution=$(lsb_release -c | awk '{print $2}')

if [ -z $mongod_path ]; then
    echo "Installing..."
    wget -qO - https://www.mongodb.org/static/pgp/server-4.2.asc | sudo apt-key add -
    echo "deb [ arch=$aarch ] https://repo.mongodb.org/apt/ubuntu $distribution/mongodb-org/4.2 multiverse" | sudo tee /etc/apt/sources.list.d/mongodb-org-4.2.list
    sudo apt-get update > /dev/null
    sudo apt-get install -y mongodb-org
else
    echo "Already installed in $mongod_path"
fi

echo "[2.1] Starting Mongo service:"

(sudo service mongodb start && echo "OK") || echo "FAILED"


echo "[3] Screen:"

screen_path=$(which screen)

if [ -z $screen_path ]; then
    echo "Installing..."
    sudo apt-get install screen
else
    echo "Already installed in $screen_path"
fi
