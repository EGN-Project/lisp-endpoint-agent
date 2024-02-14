/*
  SPDX-License-Identifier: Apache-2.0
*/

import {Object, Property} from 'fabric-contract-api';

@Object()
export class TransactionLog {
    @Property()
    public transactionID: string;

    @Property()
    public authorID: string;

    @Property()
    public time: Date;
}
