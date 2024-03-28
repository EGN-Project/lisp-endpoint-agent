#!/bin/bash

cd node
export CHAINCODE_SERVER_ADDRESS="localhost:7051"
export CHAINCODE_ID_NAME="mycc"
yarn start:server-debug