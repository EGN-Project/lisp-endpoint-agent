# EGN Project

### Functionality
The platform is designed for real-time monitoring, employing signature-based detection and machine learning algorithms to identify both known and unknown security threats. It allows for dynamic updates to threat detection rules without the need for software updates. Additionally, the system logs all activities and generates detailed reports for review by engineers.

### Usability Features
The console-based user interface is crafted for ease of use, enabling end-users to initiate manual threat scans and updates. The system provides comprehensive reports in a human-readable format, and users can customize alert levels and response actions based on their preferences.

### Safety Measures
Safety is a top priority, ensuring that the Endpoint Protection Agent's actions do not compromise the integrity and safety of monitored devices. Integration with the blockchain network provides tamper-proof audit trails for all interactions, and the system adheres to best practices for data security and encryption. Regular security audits and code reviews are conducted to address potential vulnerabilities.

### System Design and Key Subsystems
The architecture of the platform includes key subsystems like
  * Endpoint Protection Agent
  * Blockchain Integration Subsystem
  * Smart Contract Development Subsystem
  * User Interface Subsystem
  * Testing and Validation Subsystem

Each subsystem plays a crucial role in ensuring the effectiveness, reliability, and security of the entire Endpoint Protection Platform.

### How To Run
To run Test Network, refer to -> `https://docs.google.com/document/d/1cyQHG-WsbXzv9V11rWc0OUhWlItdNUve1G4DRDz7bSU/edit`

#### In terminal 1:

1. Go to fabric sample directory. 
   run:
    `./network.sh up`
    `./network.sh createChannel -c mychannel`
    `./network.sh deployCC -ccn basic -ccp ../egn-project/node -ccl typescript`

2. Then paste these commands in the terminal

    `export PATH=${PWD}/../bin:$PATH
    export FABRIC_CFG_PATH=$PWD/../config/`

   `export CORE_PEER_TLS_ENABLED=true
    export CORE_PEER_LOCALMSPID="Org1MSP"
    export CORE_PEER_TLS_ROOTCERT_FILE=${PWD}/organizations/peerOrganizations/org1.example.com/peers/peer0.org1.example.com/tls/ca.crt
    export CORE_PEER_MSPCONFIGPATH=${PWD}/organizations/peerOrganizations/org1.example.com/users/Admin@org1.example.com/msp
    export CORE_PEER_ADDRESS=localhost:7051`

    `peer chaincode invoke -o localhost:7050 --ordererTLSHostnameOverride orderer.example.com --tls --cafile      "${PWD}/organizations/ordererOrganizations/example.com/orderers/orderer.example.com/msp/tlscacerts/tlsca.example.com-cert.pem" -C mychannel -n basic --peerAddresses localhost:7051 --tlsRootCertFiles "${PWD}/organizations/peerOrganizations/org1.example.com/peers/peer0.org1.example.com/tls/ca.crt" --peerAddresses localhost:9051 --tlsRootCertFiles "${PWD}/organizations/peerOrganizations/org2.example.com/peers/peer0.org2.example.com/tls/ca.crt" -c '{"function":"InitLedger","Args":[]}'`

#### In terminal 2:
   run: 
     `./run-gateway.sh`

#### In terminal 3: 
   run the lisp console

   To run Lisp Console, ensure sbcl and quicklisp is installed on your device. Then go in the terminal and run:
    * `sbcl`
    * `(load "path to setup.lisp")`  this is found in the quicklisp folder which should be installed locally already
    * `(load "path to endpoint-console")`



## Use-case Diagram
<img width="600" src="./meta/use-case-diagram.svg" />

## Class Diagram
<img width="600" src="./meta/class-diagram.svg" />

#### Authors
- [Colton Rohan](https://github.com/ColtonRohan)
- [Emmanuel David](https://github.com/edavid2021)
- [Carlodavid Soto](https://github.com/Bebo561)
- [Vladia Zouga](https://github.com/vladiazouga)
- [Nikhil Shetty](https://github.com/nixz)
- [Yurixander Ricardo](https://github.com/yurixander)
