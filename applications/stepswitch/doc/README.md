# Stepswitch *VoIP line finder*

Stepswitch is the carrier management application, most often used for offnet routing (calls heading to upstream carriers).


## Outbound routing

In KAZOO, a carrier is represented as a "resource" document. These docs can be stored in the "global" pool (the `offnet` database) for all tenants of the cluster to use; or the docs can be stored in a particular account (so called "local" resources) for use by the tenant and/or any sub-accounts if configured to do so. Local resources are part of the BYOC (Bring Your Own Carrier) offering of KAZOO.

The `resources` callflow action can be used (generally in conjunction with the `no_match` callflow number) to route calls using local or global carriers.

## Inbound routing

Stepswitch also does inbound call number-to-account associations.

When a call comes in from a carrier to a DID, there is no account information yet associated with the call (meaning callflows, trunkstore, etc can't process the call because they can't find the relevant configurations for the receiving account). Stepswitch will normalize the dialed number and check the corresponding number document for the account id assigned to the number.

Once associated with an account, Stepswitch will replay the route request with attached account information. Callflows, trunkstore, et al can then attempt to handle the call normally.

### Authentication

Stepswitch will also perform reverse lookups based on the realm and IP of the caller. If the realm is associated with an account, that account's local resources will also be checked (if the global resource check fails to find any resources).
