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
      allResults.push(record);
      result = await iterator.next();
    }
    return JSON.stringify(allResults);
  }
}
