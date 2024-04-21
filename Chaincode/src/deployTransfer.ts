import {
  Context,
  Contract,
  Info,
  Returns,
  Transaction,
} from "fabric-contract-api";
import stringify from "json-stringify-deterministic";
import sortKeysRecursive from "sort-keys-recursive";
import { Deployment } from "./deployment";

@Info({
  title: "DeployTransfer",
  description: "Smart contract for trading deployment data",
})
export class DeployTransferContract extends Contract {
  @Transaction()
  public async InitLedger(ctx: Context): Promise<void> {
    const deployment: Deployment[] = [
      {
        authorID: "1111",
        deploymentID: "0000",
        comment: "Hello",
        payload: "sodjfo",
      },
      {
        authorID: "2222",
        deploymentID: "9999",
        comment: "Hey",
        payload: "kajsnf",
      },
      {
        authorID: "3333",
        deploymentID: "8888",
        comment: "Hi",
        payload: "khklhl",
      },
      {
        authorID: "4444",
        deploymentID: "7777",
        comment: "Hola",
        payload: "nknjnk",
      },
      {
        authorID: "5555",
        deploymentID: "6666",
        comment: "Aloha",
        payload: "lrterw",
      },
    ];

    for (const asset of deployment) {
      // example of how to write to world state deterministically
      // use convention of alphabetic order
      // we insert data in alphabetic order using 'json-stringify-deterministic' and 'sort-keys-recursive'
      // when retrieving data, in any lang, the order of data will be the same and consequently also the corresponding hash
      await ctx.stub.putState(
        asset.deploymentID,
        Buffer.from(stringify(sortKeysRecursive(asset)))
      );
      console.info(`Asset ${asset.deploymentID} initialized`);
    }
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
}
