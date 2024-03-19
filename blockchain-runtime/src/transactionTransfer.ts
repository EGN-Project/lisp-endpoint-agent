import {
  Context,
  Contract,
  Info,
  Returns,
  Transaction,
} from "fabric-contract-api";
import stringify from "json-stringify-deterministic";
import sortKeysRecursive from "sort-keys-recursive";
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
  title: "TransactionLogTransfer",
  description: "Smart contract for interacting with stored transactions",
})
export class TransactionLogTransferContract extends Contract {
  @Transaction()
  public async InitLedger(ctx: Context): Promise<void> {
    const TransactionLogs: TransactionLog[] = [
      {
        transactionID: "1",
        authorID: "Bebo",
        time: date,
        description: "Example",
      },
      {
        transactionID: "2",
        authorID: "Bebo",
        time: date,
        description: "Example",
      },
    ];

    for (const asset of TransactionLogs) {
      // example of how to write to world state deterministically
      // use convention of alphabetic order
      // we insert data in alphabetic order using 'json-stringify-deterministic' and 'sort-keys-recursive'
      // when retrieving data, in any lang, the order of data will be the same and consequently also the corresponding hash
      await ctx.stub.putState(
        asset.transactionID,
        Buffer.from(stringify(sortKeysRecursive(asset)))
      );
      console.info(`Stored Transaction ${asset.transactionID} initialized`);
    }
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

      let record: unknown;

      try {
        record = JSON.parse(strValue);
      } catch (error: unknown) {
        console.log(error);
        record = strValue;
      }

      allResults.push(record);
      result = await iterator.next();
    }

    return JSON.stringify(allResults);
  }
}
