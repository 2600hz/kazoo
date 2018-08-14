# Maintenance

## Operations

Currently the operations team must maintain the list of IP addresses.  This should be facilitated by use of the SUP commands detailed below.

### Making IPs Available
When a new block of IP addresses are acquired they should be distributed among the SBCs and added to the bind lists.  These IPs should then be added via the SUP command to the list of available IPs specifying the zone and host where the IP exists.

### Maintaining IPs
In the event of an issue, the IP summary can be used to determine:
* If the IP is assigned to an account, and if so which
* What server the IP routes to
* What zone the IP belongs to

By filtering the summary by host the assignments to a particular server can be determined and compared.  This allows rapid diagnostics and configuration reconstruction in the event an IP fails to route properly.

### Assigning IPs
At this time IP assignment remains the take of operations.  When a client needs an IP address the operations team should find one in the customers preferred zone.  Once identified the IP should be assigned to the customers account using the SUP commands below.

## SUP Commands

### Refresh the Database
This will create the dedicated_ips database, if it does not exist.  It will also overwrite or add the dedicated_ips view with the version packaged with Kazoo.

    sup kazoo_ips_maintenance refresh

### Summarize the Dedicated IPs
Displays a table of the dedicated IPs in the system, the status of each, and the account ID if assigned.  Optionally, this list can be filtered by the host (server hostname) to facilitate operations.

    sup kazoo_ips_maintenance summary [<host>]

### Add an Available IP
Add a new IP to the system.  The zone is provided so customers can ensure redundancy by purchasing dedicated IPs in different zones.  The host (server hostname) to which the IP is currently routes is used by the Operations team to quickly troubleshoot and ensure IP assignments.

    sup kazoo_ips_maintenance add <ip> <zone> <host>


### Assign an IP to an Account
This will associate an available IP to an account.

    sup kazoo_ips_maintenance assign <ip> <account>


### Release an IP Assign
This will remove the account assignment and make the IP available again.  It will remain in the system until deleted by an admin.

    sup kazoo_ips_maintenance release <ip>


### Delete an IP from the System
This will remove the IP completely from the system.

    sup kazoo_ips_maintenance delete <ip>
