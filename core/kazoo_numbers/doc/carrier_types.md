# Carrier Types

Every number in the system has an internal property called `pvt_carrier_module` that defines which carrier module is used to manage the number.  The carrier module can modify how the number behaves in the system.

## Manual Number Management

### `knm_local`

This module is responsible for "local" numbers, that is number that only belongs to one account (*local to that account*) and not to the system.
These numbers are expected to be added by the admin of the account (local admin) *for use with that account only*.
Furthermore, it is expected the number is owned by the local admin and not the system operators.
For example, if an account was using its own carriers (local resources) then all numbers would be owned by the local admin and not participate in any of the automatic facilities provided by Kazoo.

These special numbers do not behave like any other numbers in the system:

* When `released` they are completely deleted from the system, bypassing the typical lifecycle
* By default they are not quantified by billing modules
* By default they do not trigger call authorization
* By default they can not be used to route inter-account calls without leaving the cluster
* By default they can not use any number features that would incur charges/cost (such as E911 and CNAM)

### `knm_inventory`

This module is responsible for number inventories that are manually managed by the system administrator.
All numbers managed by this module will move through the typical number lifecycle.

These numbers are expected to be added via CSV jobs/tasks as available numbers, search and found by local admins and returned through aging to the available pool if released.

### `knm_managed`

This module is responsible for numbers that are generated for specific accounts by the system administrator.
All numbers managed by this module will move through the typical number lifecycle.

These numbers are expected to be added via `SUP` as available numbers that can only be searched and acquired by the pre-determined account.

Primarily, this is used for testing or development.

#### Example

Generate 5 numbers, starting at 2223334000, for `{ACCOUNT_ID}`:

```bash
sup kazoo_numbers_maintenance generate_numbers managed {ACCOUNT_ID} 2223334000 5
```

### `knm_mdn`

This module is responsible for numbers added for mobile services.
All numbers managed by this module can only be `in_service`.
They behave like `knm_local` numbers.

These numbers are internally generated and do not have a direct means of creation via the `phone_numbers` API.

## Integrations

### `knm_telnyx`

This module integrates with https://telnyx.com/ to provide number searching and provisioning services.

### `knm_vitelity`

This module integrates with http://www.vitelity.com/ to provide number searching and provisioning services.

### `knm_voip_innovations`

This module integrates with https://voipinnovations.com/ to provide number searching and provisioning services.

### `knm_bandwidth`

This module is deprecated please use `knm_bandwidth2`.

### `knm_bandwidth2`

This module integrates with https://www.bandwidth.com/ to provide number searching and provisioning services.

### `knm_inum`

This module integrates with http://www.inum.net to provide number searching and provisioning services.

### `knm_other`

This module is current a work-in-progress to provide a generic interface for midware applications.
