
### Crossbar Maintenance Functions

#### Migrate Ring Group Callflow

The `migrate_ring_group_callflow` function will migrate callflow create with a ring group by **Monster UI before v3.19**.

It will extract the ring group and create a new callflow and then reference this new callflow in the old one. This function will not impact any other callflows or ring group callflows create by Kazoo UI.
