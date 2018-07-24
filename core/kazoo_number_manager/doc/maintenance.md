# Kazoo Number Manager maintenance


## carrier_module_usage Prefix

* `Prefix`: the first few characters of an E164 number

> sup kazoo_number_manager_maintenance carrier_module_usage +1555

```
numbers%2F%2B1555:
knm_local: 7
knm_managed: 14
Totals:
knm_local: 7
knm_managed: 14
```


## carrier_module_usage

> sup kazoo_number_manager_maintenance carrier_module_usage

```
...snip...
Totals:
knm_local: 174
knm_managed: 34
knm_bandwidth2: 1
knm_telnyx: 10
knm_bandwidth: 3
knm_mdn: 1
```


## convert_carrier_module_number {NUM} Target

* `{NUM}`: a phone number convertible to E164 format
* `Target`: a valid carrier module name

> sup kazoo_number_manager_maintenance convert_carrier_module_number 4152266659 knm_bandwid

```
Bad carrier module: knm_bandwid
```

> sup kazoo_number_manager_maintenance convert_carrier_module_number 4152266659 knm_bandwidth2

```
updated carrier module to knm_bandwidth2 for 1:
+14152266659
updating carrier module failed for 0:
```


## convert_carrier_module Source Target Prefix

* `Source`: the carrier to match with (does not need to be valid)
* `Target`: a valid carrier module name
* `Prefix`: the first few characters of an E164 number

> sup kazoo_number_manager_maintenance convert_carrier_module knm_band knm_bandwidth2 +1415

```
attempt to convert numbers with carrier module knm_band to knm_bandwidth2 in database numbers%2F%2B1415
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
```


## convert_carrier_module Source Target

* `Source`: the carrier to match with (does not need to be valid)
* `Target`: a valid carrier module name

> sup kazoo_number_manager_maintenance convert_carrier_module knm_bandwidth knm_bandwidth2

```
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1202
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1236
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1248
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1312
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1315
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1318
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1414
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1415
updated carrier module to knm_bandwidth2 for 2:
+14152338397
+14152338421
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1424
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1425
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1434
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1454
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1464
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1497
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1498
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1499
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1504
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1555
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1665
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1666
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1800
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1937
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B3980
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4242
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4252
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4262
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4411
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4412
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4413
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4414
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4415
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4416
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4417
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4418
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4419
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4420
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4423
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4424
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4428
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4429
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4433
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4470
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4480
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4484
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4487
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B8835
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
```


## reset_allowed_features_to_defaults_on_system_config

> sup kazoo_number_manager_maintenance reset_allowed_features_to_defaults_on_system_config

```
Features allowed on system config document:
	carrier_name cnam e911 failover force_outbound inbound_cnam outbound_cnam port prepend ringback
```
