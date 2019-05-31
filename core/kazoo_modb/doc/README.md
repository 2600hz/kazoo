# Kazoo Month Only DB *Manages Monthly Databases*

Month-only databases are used to separate account configuration settings (like users, devices, callflows, etc) and temporal data like call recordings, faxes, voicemails, etc. This allows the account database to stay pretty small in size while the MODBs can grow based on usage but once a new month rolls around, cap the database's size and allow it to be archived and removed from the DB cluster after a certain time.

## Intialization

MODBs are loaded with the appropriate views then a series of routines are run. These are set in the `system_config/modb` document under the `routines` key.
