import { Object, Property } from "fabric-contract-api";

@Object()
export class Deployment {
  @Property()
  public payload: string;

  @Property()
  public comment: string;

  @Property()
  public deploymentID: string;

  @Property()
  public authorID: string;
}
