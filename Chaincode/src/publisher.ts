import { Object, Property } from "fabric-contract-api";

@Object()
export class Publisher {
  @Property()
  public vendorID: PublicKeyCredentialType;
}
