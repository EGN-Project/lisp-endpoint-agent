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

function getCurrentTimestamp() {
  return Math.floor(Date.now() / 1000); // Convert milliseconds to seconds
}

function toDate(timestamp: typeof TIMESTAMP) {
  const milliseconds =
    (timestamp.seconds.low + timestamp.nanos / 1000000 / 1000) * 1000;

  return new Date(milliseconds);
}

function generateUniqueId() {
  return Date.now().toString(36) + Math.random().toString(36).substr(2);
}


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

  //This function handles a revocation of a deployment through being passed a deploymentID
  @Transaction()
  public async Revoke(
    ctx: Context, 
    deploymentID: string,
    reason: string,
    authorID: string
  ): Promise<String>{
    const exists = await this.ValidateDeployment(ctx, deploymentID);
     if (!exists) {
       throw new Error(`The does not exist, or already has been revoked`);
     }
     const id = generateUniqueId();
     const revocation = {
      targetDeploymentID: deploymentID,
      reason: reason,
      revocationID: id
     }

     const currentTimestamp = getCurrentTimestamp();

      const TIMESTAMP = {
          seconds: {
              low: currentTimestamp,
              high: 0,
              unsigned: false,
          },
          nanos: 0, // Reset nanoseconds to 0 for simplicity
      } as const;

      const date = toDate(TIMESTAMP);

     this.CreateAsset(ctx, deploymentID, authorID, date, reason);
     await ctx.stub.putState(
      deploymentID,
      Buffer.from(stringify(sortKeysRecursive(revocation)))
    );

    await ctx.stub.deleteState(deploymentID);

    const jsonResponse = JSON.stringify({
      status: "success",
      message: `Deployment with ID ${deploymentID} revoked successfully`,
    });

    return jsonResponse;
  }

  @Transaction()
  public async Deployment(
    ctx: Context,
    authorID: string,
    comment: string,
    payload: string,
    deploymentID: string
  ): Promise<string> {
    const exists = await this.ValidateDeployment(ctx, deploymentID);
    if (exists) {
      throw new Error(`The asset ${deploymentID} already exists`);
    }
  
    const currentTimestamp = getCurrentTimestamp();
  
    const TIMESTAMP = {
      seconds: {
        low: currentTimestamp,
        high: 0,
        unsigned: false,
      },
      nanos: 0, // Reset nanoseconds to 0 for simplicity
    } as const;
  
    const date = toDate(TIMESTAMP);
  
    // Create a log of the deployment action
    await this.CreateAsset(ctx, deploymentID, authorID, date, comment);
  
    // Store deployment details in the ledger
    const deployment = {
      deploymentID: deploymentID,
      comment: comment,
      authorID: authorID,
      payload: payload,
      time: date // Include timestamp in deployment object
    };
  
    await ctx.stub.putState(
      deploymentID,
      Buffer.from(stringify(sortKeysRecursive(deployment)))
    );
  
    const jsonResponse = JSON.stringify({
      status: "success",
      message: `Deployment with ID ${deploymentID} successfully posted`,
    });
  
    return jsonResponse;
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
    console.log(ID);
    let id : string = generateUniqueId();
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
        //record = strValue;
      }
      if(record.transactionID !== undefined){
        allResults.push(record);
      }
      
      result = await iterator.next();
    }

    return JSON.stringify(allResults);
  }
}
