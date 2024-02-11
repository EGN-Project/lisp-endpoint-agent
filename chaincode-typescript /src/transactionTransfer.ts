/*
 * SPDX-License-Identifier: Apache-2.0
 */
// Deterministic JSON.stringify()
import {Context, Contract, Info, Returns, Transaction} from 'fabric-contract-api';
import stringify from 'json-stringify-deterministic';
import sortKeysRecursive from 'sort-keys-recursive';
import {StoredTransaction} from './transaction';

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

@Info({title: 'StoredTransactionTransfer', description: 'Smart contract for interacting with stored transactions'})
export class StoredTransactionTransferContract extends Contract {

    @Transaction()
    public async InitLedger(ctx: Context): Promise<void> {
        const storedTransactions: StoredTransaction[] = [
            {
                ID: '1',
                authorID: 'Bebo',
                time: date,
            },
            {
                ID: '2',
                authorID: 'Bebo',
                time: date
            },
        ];

        for (const asset of storedTransactions) {
            // example of how to write to world state deterministically
            // use convetion of alphabetic order
            // we insert data in alphabetic order using 'json-stringify-deterministic' and 'sort-keys-recursive'
            // when retrieving data, in any lang, the order of data will be the same and consequently also the corresonding hash
            await ctx.stub.putState(asset.ID, Buffer.from(stringify(sortKeysRecursive(asset))));
            console.info(`Stored Transaction ${asset.ID} initialized`);
        }
    }

    // Stored TransactionExists returns true when asset with given ID exists in world state.
    @Transaction(false)
    @Returns('boolean')
    public async TransactionExists(ctx: Context, id: string): Promise<boolean> {
        const transactionJSON = await ctx.stub.getState(id);
        return transactionJSON && transactionJSON.length > 0;
    }


}
