# Shipyard

Shipyard is the build pipeline for KAZOO and other projects, generating artifacts like RPMs for use in building clusters.

## Per-application config

Each KAZOO application should define a `.shipyard.yml` file structured to provide the relevant metadata and dependencies.

### Description

The description key should be constrained to 80-character columns
