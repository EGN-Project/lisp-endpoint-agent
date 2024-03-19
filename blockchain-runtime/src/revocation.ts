import { Object, Property } from "fabric-contract-api";

@Object()
export class Revocation {
  @Property()
  public targetDeploymentID: string;

  @Property()
  public reason: string;

  @Property()
  public RevocationID: string;
}
