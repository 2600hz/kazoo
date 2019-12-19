# Trunkstore *Lightweight trunking services*

Trunkstore provides SIP connectivity to existing PBXs. In contrast to the callflow application, trunkstore minimally processes calls it is configured to handle, with limited features available.

## Inbound

Calls from upstream carriers (or other accounts on the cluster) will be connected to the associated SIP configuration in the account that is assigned the dialed number.

The only feature available is to define a failover number. If the PBX is unable to answer the call, the failover route (a SIP address or DID) will be attempted.

## Outbound

Calls from the configured PBX will be routed to either the account's local resources or the cluster's global resources pool.

No additional features are provided in this scenario.
