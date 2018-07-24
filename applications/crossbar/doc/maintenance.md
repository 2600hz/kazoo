
### Crossbar Maintenance Functions

#### Migrate Ring Group Callflow

The `migrate_ring_group_callflow` function will migrate callflow create with a ring group by **Monster UI before v3.19**.

It will extract the ring group and create a new callflow and then reference this new callflow in the old one. This function will not impact any other callflows or ring group callflows create by Kazoo UI.


### Update UI apps metadata

#### List all apps

```shell
sup crossbar_maintenance apps
```

#### Get one app's metadata

```shell
sup crossbar_maintenance app AppName_Or_AppId
```

#### Update an app's en-US fields

```shell
sup crossbar_maintenance set_app_label AppId NewValue
```

```shell
sup crossbar_maintenance set_app_description AppId NewValue
```

```shell
sup crossbar_maintenance set_app_extended_description AppId NewValue
```

Note: features must be @-separated if more than one is supplied.

```shell
sup crossbar_maintenance set_app_features AppId 'my feature'
sup crossbar_maintenance set_app_features AppId feature1@featureB@feat3
```

#### Update an app's icon

```shell
sup crossbar_maintenance set_app_icon AppId PathToPNGIcon
```

#### Update an app's screenshots

```shell
sup crossbar_maintenance set_app_screenshots AppId PathToScreenshotsFolder
```
