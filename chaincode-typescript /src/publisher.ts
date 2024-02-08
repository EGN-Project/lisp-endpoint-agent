/*
  SPDX-License-Identifier: Apache-2.0
*/

import {Object, Property} from 'fabric-contract-api';
import { Timestamp } from 'fabric-shim';

@Object()
export class Transaction {
    @Property()
    public vendorID: PublicKeyCredentialType;
}
