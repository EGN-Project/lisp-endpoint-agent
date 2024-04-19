import * as grpc from '@grpc/grpc-js';
import { connect, Contract, Identity, Signer, signers } from '@hyperledger/fabric-gateway';
import * as crypto from 'crypto';
import { promises as fs } from 'fs';
import * as path from 'path';
import { TextDecoder } from 'util';
import express from 'express';
import bodyParser from 'body-parser';

const channelName = envOrDefault('CHANNEL_NAME', 'mychannel');
const chaincodeName = envOrDefault('CHAINCODE_NAME', 'basic');
const mspId = envOrDefault('MSP_ID', 'Org1MSP');

// Path to crypto materials.
const cryptoPath = envOrDefault('CRYPTO_PATH', path.resolve(__dirname, '..', '..', '..', 'test-network', 'organizations', 'peerOrganizations', 'org1.example.com'));

// Path to user private key directory.
const keyDirectoryPath = envOrDefault('KEY_DIRECTORY_PATH', path.resolve(cryptoPath, 'users', 'User1@org1.example.com', 'msp', 'keystore'));

// Path to user certificate directory.
const certDirectoryPath = envOrDefault('CERT_DIRECTORY_PATH', path.resolve(cryptoPath, 'users', 'User1@org1.example.com', 'msp', 'signcerts'));

// Path to peer tls certificate.
const tlsCertPath = envOrDefault('TLS_CERT_PATH', path.resolve(cryptoPath, 'peers', 'peer0.org1.example.com', 'tls', 'ca.crt'));

// Gateway peer endpoint.
const peerEndpoint = envOrDefault('PEER_ENDPOINT', 'localhost:7051');

// Gateway peer SSL host name override.
const peerHostAlias = envOrDefault('PEER_HOST_ALIAS', 'peer0.org1.example.com');

const utf8Decoder = new TextDecoder();
const app = express();
const port = 3000;
app.use(bodyParser.json());

var close = false;

// Parse URL-encoded bodies
app.use(bodyParser.urlencoded({ extended: true }));
const deploymentID = `deployment${Date.now()}`;
const revocationID = `revocation${Date.now()}`;

async function main(): Promise<void> {
  await displayInputParameters();

  // The gRPC client connection should be shared by all Gateway connections to this endpoint.
  const client = await newGrpcConnection();

  const gateway = connect({
      client,
      identity: await newIdentity(),
      signer: await newSigner(),
      // Default timeouts for different gRPC calls
      evaluateOptions: () => {
          return { deadline: Date.now() + 5000 }; // 5 seconds
      },
      endorseOptions: () => {
          return { deadline: Date.now() + 15000 }; // 15 seconds
      },
      submitOptions: () => {
          return { deadline: Date.now() + 5000 }; // 5 seconds
      },
      commitStatusOptions: () => {
          return { deadline: Date.now() + 60000 }; // 1 minute
      },
  });

  try {
    // Get a network instance representing the channel where
    // the smart contract is deployed.
    const network = gateway.getNetwork(channelName);

    // Get the smart contract from the network.
    const contract = network.getContract(chaincodeName);

    // Initialize a set of asset data on the ledger using
    // the chaincode 'InitLedger' function.
    await initLedger(contract);

    app.post("/deploy", async (req, res) => {
      try {
        // Extract data from the request body.
        console.log(req.body);

        const authorID = req.body.authorID;
        const comment = req.body.comment;
        const payload = req.body.payload;
        const deploymentID = req.body.deploymentID

        console.log(
          "\n--> Submit Transaction: Deploy, creates new deployment with ID, Description, Author, and Code"
        );

        await contract.submitTransaction(
          "Deployment",
          authorID,
          comment,
          payload,
          deploymentID
        );

        console.log("*** Transaction committed successfully");
        gateway.close();
        res.status(200).json({ message: "Deployment successful" });
      } catch (error) {
        console.error("Error deploying asset:", error);
        res.status(500).json({ error: "Failed to deploy asset" });
      }
    });

    // Post for get deployment by id.
    app.post("/getDeploymentByID", async (req, res) => {
      try {
        const { deploymentID } = req.body;
        console.log(req.body)
        console.log(
          "\n--> Evaluate Transaction: ReadAsset, function returns asset attributes"
        );

        const resultBytes = await contract.evaluateTransaction(
          "GetDeploymentByID",
          deploymentID
        );

        const resultJson = utf8Decoder.decode(resultBytes);
        const result = JSON.parse(resultJson);

        console.log("*** Result:", result);
        gateway.close();
        res.status(200).json({ message: "Deployment retrieved successfully" });
      } catch (error) {
        console.error("Error retrieving deployment:", error);
        res.status(500).json({ error: "Failed to retrieve deployment" });
      }
    });

    // Post for remove Deployment By ID.
    app.delete("/revokeDeployment", async (req, res) => {
      try {
        // Extract data from the request body
        const { deploymentID } = req.body;

        console.log(
          "\n--> Evaluate Transaction: ReadAsset, function returns asset attributes"
        );

        const resultBytes = await contract.evaluateTransaction(
          "RevokeDeployment",
          deploymentID
        );

        const resultJson = utf8Decoder.decode(resultBytes);
        const result = JSON.parse(resultJson);

        console.log("*** Result:", result);
        gateway.close();
        res.status(200).json({ message: "Deployment retrieved successfully" });
      } catch (error) {
        console.error("Error retrieving deployment:", error);
        res.status(500).json({ error: "Failed to retrieve deployment" });
      }
    });

    // GET endpoint for retrieving all revocations
    app.get("/getAllRevocations", async (_req, res) => {
      try {
        // Call the getAllRevocations function
        console.log(
          "\n--> Evaluate Transaction: GetAllRevocations, function returns all the current Revocations on the ledger"
        );

        const resultBytes = await contract.evaluateTransaction(
          "GetAllRevocations"
        );

        const resultJson = utf8Decoder.decode(resultBytes);
        const result = JSON.parse(resultJson);

        console.log("*** Result:", result);
        gateway.close();

        res
          .status(200)
          .json({ message: "Retrieved all revocations successfully" , data: result});
      } catch (error) {
        console.error("Error retrieving revocations:", error);
        res.status(500).json({ error: "Failed to retrieve revocations" });
      }
    });

    // Post for get revocation by id
    app.post("/getRevocationByID", async (req, res) => {
      try {
        // Extract data from the request body
        const { revocationID } = req.body;
        console.log(revocationID)
        console.log(
          "\n--> Evaluate Transaction: ReadAsset, function returns asset attributes"
        );

        const resultBytes = await contract.evaluateTransaction(
          "GetRevocationByID",
          revocationID
        );

        const resultJson = utf8Decoder.decode(resultBytes);
        const result = JSON.parse(resultJson);

        console.log("*** Result:", result);
        gateway.close();
        res.status(200).json({ message: "Revocation retrieved successfully", data: result });
      } catch (error) {
        console.error("Error retrieving revocation:", error);
        res.status(500).json({ error: "Failed to retrieve revocation" });
      }
    });

    // Post for validate revocation.
    app.post("/validateRevocation", async (req, res) => {
      try {
        // Extract data from the request body
        const { revocationID } = req.body;
        console.log(req.body)
        console.log(revocationID);
        console.log(
          "\n--> Evaluate Transaction: ValidateRevocation, function returns true if revocation exists"
        );

        const resultBytes = await contract.evaluateTransaction(
          "ValidateRevocation",
          revocationID
        );

        const resultJson = utf8Decoder.decode(resultBytes);
        const result = JSON.parse(resultJson);

        console.log("*** Result:", result);
        gateway.close();
        res.status(200).json({ message: "Revocation validated successfully" });
      } catch (error) {
        console.error("Error validating revocation:", error);
        res.status(500).json({ error: "Failed to validate revocation" });
      }
    });

    // GET endpoint for retrieving all transaction logs
    app.get("/transaction-logs", async (_req, res) => {
      try {
        console.log(
          "\n--> Evaluate Transaction: GetAllDeployments, function returns all the current deployments on the ledger"
        );

        const resultBytes = await contract.evaluateTransaction(
          "GetAllTransactionLogs"
        );

        const resultJson = utf8Decoder.decode(resultBytes);
        const result = JSON.parse(resultJson);

        console.log("*** Result:", result);
        gateway.close();

        res
          .status(200)
          .json({ message: "Retrieved all transaction logs successfully",
                  data: result });
      } catch (error) {
        console.error("Error retrieving transaction logs:", error);
        res.status(500).json({ error: "Failed to retrieve transaction logs" });
      }
    });
  } catch {
    console.log("error");
  }
  while(close){
    console.log("Closing");
    gateway.close();
    client.close();
  }
}

main().catch(error => {
  console.error('******** FAILED to run the application:', error);
  process.exitCode = 1;
});

async function newGrpcConnection(): Promise<grpc.Client> {
  const tlsRootCert = await fs.readFile(tlsCertPath);
  const tlsCredentials = grpc.credentials.createSsl(tlsRootCert);
  return new grpc.Client(peerEndpoint, tlsCredentials, {
      'grpc.ssl_target_name_override': peerHostAlias,
  });
}

async function newIdentity(): Promise<Identity> {
  const certPath = await getFirstDirFileName(certDirectoryPath);
  const credentials = await fs.readFile(certPath);
  return { mspId, credentials };
}

async function getFirstDirFileName(dirPath: string): Promise<string> {
  const files = await fs.readdir(dirPath);
  return path.join(dirPath, files[0]);
}

async function newSigner(): Promise<Signer> {
  const keyPath = await getFirstDirFileName(keyDirectoryPath);
  const privateKeyPem = await fs.readFile(keyPath);
  const privateKey = crypto.createPrivateKey(privateKeyPem);
  return signers.newPrivateKeySigner(privateKey);
}

/**
 * This type of transaction would typically only be run once by an application the first time it was started after its
 * initial deployment. A new version of the chaincode deployed later would likely not need to run an "init" function.
 */
 async function initLedger(contract: Contract): Promise<void> {
  console.log('\n--> Submit Transaction: InitLedger, function creates the initial set of assets on the ledger');

  await contract.submitTransaction('InitLedger');

  console.log('*** Transaction committed successfully');
}


/**
 * Submit transaction asynchronously, allowing the application to process the smart contract response (e.g. update a UI)
 * while waiting for the commit notification.
 */
async function transferAssetAsync(contract: Contract): Promise<void> {
  console.log(
    "\n--> Async Submit Transaction: TransferAsset, updates existing asset owner"
  );

  const commit = await contract.submitAsync("TransferAsset", {
    arguments: [deploymentID, "Saptha"],
  });

  const oldOwner = utf8Decoder.decode(commit.getResult());

  console.log(
    `*** Successfully submitted transaction to transfer ownership from ${oldOwner} to Saptha`
  );

  console.log("*** Waiting for transaction commit");

  const status = await commit.getStatus();

  if (!status.successful) {
    throw new Error(
      `Transaction ${status.transactionId} failed to commit with status code ${status.code}`
    );
  }

  console.log("*** Transaction committed successfully");
}

async function getAllAssets(contract: Contract): Promise<void> {
  console.log('\n--> Evaluate Transaction: GetAllAssets, function returns all the current assets on the ledger');

  const resultBytes = await contract.evaluateTransaction('GetAllAssets');

  const resultJson = utf8Decoder.decode(resultBytes);
  const result = JSON.parse(resultJson);
  console.log('*** Result:', result);
}

/**
 * submitTransaction() will throw an error containing details of any error responses from the smart contract.
 */
async function updateNonExistentAsset(contract: Contract): Promise<void> {
  console.log(
    "\n--> Submit Transaction: UpdateAsset asset70, asset70 does not exist and should return an error"
  );

  try {
    await contract.submitTransaction(
      "UpdateAsset",
      "asset70",
      "blue",
      "5",
      "Tomoko",
      "300"
    );
    console.log("******** FAILED to return an error");
  } catch (error) {
    console.log("*** Successfully caught the error: \n", error);
  }
}

/**
 * envOrDefault() will return the value of an environment variable, or a default value if the variable is undefined.
 */
 function envOrDefault(key: string, defaultValue: string): string {
  return process.env[key] || defaultValue;
}

/**
* displayInputParameters() will print the global scope parameters used by the main driver routine.
*/
async function displayInputParameters(): Promise<void> {
  console.log(`channelName:       ${channelName}`);
  console.log(`chaincodeName:     ${chaincodeName}`);
  console.log(`mspId:             ${mspId}`);
  console.log(`cryptoPath:        ${cryptoPath}`);
  console.log(`keyDirectoryPath:  ${keyDirectoryPath}`);
  console.log(`certDirectoryPath: ${certDirectoryPath}`);
  console.log(`tlsCertPath:       ${tlsCertPath}`);
  console.log(`peerEndpoint:      ${peerEndpoint}`);
  console.log(`peerHostAlias:     ${peerHostAlias}`);
}

app.listen(port, () => {
  console.log(`Server listening at http://localhost:${port}`);
});
