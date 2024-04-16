import {
  Context,
  Contract,
  Info,
  Returns,
  Transaction,
} from "fabric-contract-api";
import stringify from "json-stringify-deterministic";
import sortKeysRecursive from "sort-keys-recursive";
import { Revocation } from "./revocation";
import { Deployment } from "./deployment";
import { TransactionLog } from "./transactionLog";

const TIMESTAMP = {
  seconds: {
    low: 1659172409, // Example value for seconds
    high: 0, // High precision part, typically 0 for Hyperledger Fabric timestamps
    unsigned: false, // Whether the value is unsigned or not
  },
  nanos: 567000000, // Example value for nanoseconds
} as const;

function toDate(timestamp: typeof TIMESTAMP) {
  const milliseconds =
    (timestamp.seconds.low + timestamp.nanos / 1000000 / 1000) * 1000;

  return new Date(milliseconds);
}

const date = toDate(TIMESTAMP);

console.log(date);

@Info({
  title: "AssetTransfer",
  description: "Smart contract for trading assets",
})
export class AssetTransferContract extends Contract {
  @Transaction()
  public async InitLedger(ctx: Context): Promise<void> {
    const revocations: Revocation[] = [
      {
        targetDeploymentID: "123",
        reason: "example",
        RevocationID: "1",
      },
    ];

    for (const revocation of revocations) {
      // example of how to write to world state deterministically
      // use convetion of alphabetic order
      // we insert data in alphabetic order using 'json-stringify-deterministic' and 'sort-keys-recursive'
      // when retrieving data, in any lang, the order of data will be the same and consequently also the corresonding hash
      await ctx.stub.putState(
        revocation.RevocationID,
        Buffer.from(stringify(sortKeysRecursive(revocation)))
      );
      console.info(`Asset ${revocation.RevocationID} initialized`);
    }
  }

  // Returns Revocation By Given ID
  @Transaction(false)
  public async GetRevocationByID(
    ctx: Context,
    RevocationID: string
  ): Promise<string> {
    const assetJSON = await ctx.stub.getState(RevocationID); // get the asset from chaincode state
    if (!assetJSON || assetJSON.length === 0) {
      throw new Error(`The asset ${RevocationID} does not exist`);
    }
    return assetJSON.toString();
  }

  // AssetExists returns true when asset with given ID exists in world state.
  @Transaction(false)
  @Returns("boolean")
  public async ValidateRevocation(
    ctx: Context,
    RevocationID: string
  ): Promise<boolean> {
    const assetJSON = await ctx.stub.getState(RevocationID);
    return assetJSON && assetJSON.length > 0;
  }

  //Get all revocations
  @Transaction(false)
  @Returns("string")
  public async GetAllRevocations(ctx: Context): Promise<string> {
    const allResults = [];
    // range query with empty string for startKey and endKey does an open-ended query of all assets in the chaincode namespace.
    const iterator = await ctx.stub.getStateByRange("", "");
    let result = await iterator.next();
    while (!result.done) {
      const strValue = Buffer.from(result.value.value.toString()).toString(
        "utf8"
      );
      let record;
      try {
        record = JSON.parse(strValue);

      } catch (err) {
        console.log(err);
        record = strValue;
      }
      if(record.RevocationID !== undefined){
        allResults.push(record);
      }
      result = await iterator.next();
    }
    return JSON.stringify(allResults);
  }

   // CreateAsset issues a new asset to the world state with given details.
   @Transaction()
   public async Deployment(
     ctx: Context,
     authorID: string,
     comment: string,
     payload: string,
     deploymentID: string
   ): Promise<void> {
     const exists = await this.ValidateDeployment(ctx, deploymentID);
     if (exists) {
       throw new Error(`The asset ${deploymentID} already exists`);
     }
 
     const deployment = {
       deploymentID: deploymentID,
       comment: comment,
       authorID: authorID,
       payload: payload,
     };
     // we insert data in alphabetic order using 'json-stringify-deterministic' and 'sort-keys-recursive'
     await ctx.stub.putState(
       deploymentID,
       Buffer.from(stringify(sortKeysRecursive(deployment)))
     );
   }
 
   // ReadAsset returns the asset stored in the world state with given id.
   @Transaction(false)
   public async GetDeploymentByID(
     ctx: Context,
     deploymentID: string
   ): Promise<string> {
     const deploymentJSON = await ctx.stub.getState(deploymentID); // get the asset from chaincode state
     if (!deploymentJSON || deploymentJSON.length === 0) {
       throw new Error(`The asset ${deploymentID} does not exist`);
     }
     return deploymentJSON.toString();
   }
 
   // AssetExists returns true when asset with given ID exists in world state.
   @Transaction(false)
   @Returns("boolean")
   public async ValidateDeployment(
     ctx: Context,
     deploymentID: string
   ): Promise<boolean> {
     const deploymentJSON = await ctx.stub.getState(deploymentID);
     return deploymentJSON && deploymentJSON.length > 0;
   }
 
   @Transaction()
   public async RevokeDeployment(
     ctx: Context,
     deploymentID: string
   ): Promise<void> {
     const deploymentExists = await this.ValidateDeployment(ctx, deploymentID);
     if (!deploymentExists) {
       throw new Error(`Deployment with ID ${deploymentID} does not exist`);
     }
 
     // Delete the deployment from the ledger
     await ctx.stub.deleteState(deploymentID);
   }

   // Stored TransactionExists returns true when asset with given ID exists in world state.
  @Transaction(false)
  @Returns("boolean")
  public async TransactionExists(ctx: Context, id: string): Promise<boolean> {
    const transactionJSON = await ctx.stub.getState(id);
    return transactionJSON && transactionJSON.length > 0;
  }

  @Transaction()
  public async CreateAsset(
    ctx: Context,
    ID: string,
    authorID: string,
    time: Date,
    description: string
  ): Promise<void> {
    const exists = await this.TransactionExists(ctx, ID);

    if (exists) {
      throw new Error(`The asset ${ID} already exists`);
    }

    const transactionLog = {
      transactionID: ID,
      authorID,
      time: time,
      description: description,
    };

    // Insert data in alphabetic order using 'json-stringify-deterministic' and 'sort-keys-recursive'.
    await ctx.stub.putState(
      ID,
      Buffer.from(stringify(sortKeysRecursive(transactionLog)))
    );
  }

  // Retrieves a given transaction by its ID.
  @Transaction(false)
  public async ReadAsset(ctx: Context, id: string): Promise<string> {
    // Get the asset from chaincode state.
    const transactionLogJSON = await ctx.stub.getState(id);

    if (!transactionLogJSON || transactionLogJSON.length === 0) {
      throw new Error(
        `The log of a transaction with an ID of ${id} does not exist`
      );
    }

    return transactionLogJSON.toString();
  }

  // Returns all logs stored in blockchain
  @Transaction(false)
  @Returns("string")
  public async GetAllTransactionLogs(ctx: Context): Promise<string> {
    const allResults = [];

    // Range query with empty string for startKey and endKey does
    // an open-ended query of all assets in the chaincode namespace.
    const iterator = await ctx.stub.getStateByRange("", "");

    let result = await iterator.next();

    while (!result.done) {
      const strValue = Buffer.from(result.value.value.toString()).toString(
        "utf8"
      );

      let record: TransactionLog;

      try {
        record = JSON.parse(strValue);
      } catch (error: unknown) {
        console.log(error);
        record = strValue;
      }
      if(record.transactionID !== undefined){
        allResults.push(record);
      }
      result = await iterator.next();
    }

    return JSON.stringify(allResults);
  }
}
