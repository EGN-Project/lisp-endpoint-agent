import { Object, Property } from "fabric-contract-api";

enum Role {
  Vendor,
  CompanyAdmin,
}

@Object()
export class Governance {
  @Property()
  public address: PublicKeyCredentialType;

  @Property()
  public role: Role;
}
