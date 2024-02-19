/*
  SPDX-License-Identifier: Apache-2.0
*/

import {Object, Property} from 'fabric-contract-api';
import { Timestamp } from 'fabric-shim';

@Object()
export class Deployment {
    @Property()
    public payload: string;

    @Property()
    public comment: string;

    @Property()
    public deploymentID: string;

    @Property()
    public offerID: string;
}
