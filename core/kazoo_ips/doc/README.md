# Kazoo IP Addresses *Manages Dedicated IP Addresses*

## Purpose
The purpose of the Kazoo IPs library is to track IP addresses owned by system administrators and provided to clients for inbound routing assignments.

## Overview
The Kazoo IPs library creates and manages a database called dedicated_ips.  Each document of this database represents an IP address added by the system administrators and potentially assigned to accounts.  These assignments are used by the account admins to route inbound numbers when using their own carriers.

## Configuration
The Kazoo IPs library can be configured on the system_config/ips document.


The configuration parameter `zone_name_map` allows system administrators to map the internal zone name to a more friendly zone name that might be used on the dedicated IPs.  For example, if a cluster was comprised of `zone_1` and `zone_2` but the dedicated IPs where using terminology such as `west` or `east` this parameter should be used to make the associations.  An example using these values would be:

```
   "default": {
       "zone_name_map": {
           "zone_1": "east",
           "zone_2": "west"
       }
   },
```
