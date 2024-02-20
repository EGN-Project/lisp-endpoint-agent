/*
 * SPDX-License-Identifier: Apache-2.0
 */
// Deterministic JSON.stringify()
import {Context, Contract, Info, Returns, Transaction} from 'fabric-contract-api';
import stringify from 'json-stringify-deterministic';
import sortKeysRecursive from 'sort-keys-recursive';
import {TransactionLog} from './transactionLog';

const timestamp = {
    seconds: {
      low: 1659172409, // Example value for seconds
      high: 0, // High precision part, typically 0 for Hyperledger Fabric timestamps
      unsigned: false // Whether the value is unsigned or not
    },
    nanos: 567000000 // Example value for nanoseconds
  };
  

  function toDate(timestamp) {
    const milliseconds = (timestamp.seconds.low + ((timestamp.nanos / 1000000) / 1000)) * 1000;
    return new Date(milliseconds);
  }

  const date = toDate(timestamp);
  console.log(date)

@Info({title: 'TransactionLogTransfer', description: 'Smart contract for interacting with stored transactions'})
export class TransactionLogTransferContract extends Contract {

    @Transaction()
    public async InitLedger(ctx: Context): Promise<void> {
        const TransactionLogs: TransactionLog[] = [
            {
                transactionID: '1',
                authorID: 'Bebo',
                time: date,
                description: "Example"
            },
            {
                transactionID: '2',
                authorID: 'Bebo',
                time: date,
                description: "Example"
            },
        ];

        for (const asset of TransactionLogs) {
            // example of how to write to world state deterministically
            // use convetion of alphabetic order
            // we insert data in alphabetic order using 'json-stringify-deterministic' and 'sort-keys-recursive'
            // when retrieving data, in any lang, the order of data will be the same and consequently also the corresonding hash
            await ctx.stub.putState(asset.transactionID, Buffer.from(stringify(sortKeysRecursive(asset))));
            console.info(`Stored Transaction ${asset.transactionID} initialized`);
        }
    }

    // Stored TransactionExists returns true when asset with given ID exists in world state.
    @Transaction(false)
    @Returns('boolean')
    public async TransactionExists(ctx: Context, id: string): Promise<boolean> {
        const transactionJSON = await ctx.stub.getState(id);
        return transactionJSON && transactionJSON.length > 0;
    }

    @Transaction()
    public async CreateAsset(ctx: Context, ID: string, authorID: string, time: Date, description: string): Promise<void> {
        const exists = await this.TransactionExists(ctx, ID);
        if (exists) {
            throw new Error(`The asset ${ID} already exists`);
        }

        const transactionLog = {
            transactionID: ID,
            authorID,
            time: time,
            description: description
        };
        // we insert data in alphabetic order using 'json-stringify-deterministic' and 'sort-keys-recursive'
        await ctx.stub.putState(ID, Buffer.from(stringify(sortKeysRecursive(transactionLog))));
    }

    //Retrieves a given transaction by its ID
    @Transaction(false)
    public async ReadAsset(ctx: Context, id: string): Promise<string> {
        const transactionLogJSON = await ctx.stub.getState(id); // get the asset from chaincode state
        if (!transactionLogJSON || transactionLogJSON.length === 0) {
            throw new Error(`The log of a transaction with an ID of ${id} does not exist`);
        }
        return transactionLogJSON.toString();
    }

    // Returns all logs stored in blockchain
    @Transaction(false)
    @Returns('string')
    public async GetAllTransactionLogs(ctx: Context): Promise<string> {
        // range query with empty string for startKey and endKey does an open-ended query of all assets in the chaincode namespace.
        const iterator = await ctx.stub.getStateByPartialCompositeKey('TransactionLog', []);
        var transactionLogs: TransactionLog[] = [];

        // Convert StateQueryIterator to async iterator
        const asyncIterator = iterator[Symbol.asyncIterator]();
         // Iterate over the result set and deserialize each TransactionLog object
        for await (const result of asyncIterator) {
            const value = result.value.toString('utf-8');
            const transactionLog: TransactionLog = JSON.parse(value);
            transactionLogs.push(transactionLog);
        }

        return JSON.stringify(transactionLogs);
    }

}
