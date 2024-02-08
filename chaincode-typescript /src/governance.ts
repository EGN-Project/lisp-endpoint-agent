/*
  SPDX-License-Identifier: Apache-2.0
*/

import {Object, Property} from 'fabric-contract-api';
import { Timestamp } from 'fabric-shim';

enum Role{
  Vendor,
  CompanyAdmin
}

@Object()
export class Governance {
    @Property()
    public Address: PublicKeyCredential;

    @Property()
    public role: Role;
}
