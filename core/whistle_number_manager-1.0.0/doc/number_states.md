/*
Section: Whistle Number Manager
Title: Number States
Language: en-US
Version: 3.20
*/

Numbers travel through a variety of states within Kazoo during their lifecycle.

Let's enumerate those states and then see how they fit together!

## Number States

### Discovery

An internal number state used to temporarily hold numbers that are available for users to acquire from the system.

* Populated by API requests to activate numbers for an account that don't yet exist in the system
* Numbers in `discovery` do not participate in number hunting
* No number can transition from another state to `discovery`
* There are no public fields for `discovery` numbers
* Numbers in `discovery` are automatically purged from the database after an aging period
    * `system_config/number_manager.discovery_expiry_d`: How many days a number in the `discovery` state will remain before being deleted from the database.

### Port In

When an account initiates a port request via the APIs, the number is created in the `port_in` state.

* Numbers in `port_in` will automatically transition to `in_service` the first time a call is received from a carrier to that number (the number hunt process)
* Only used for internal number hunting within an account; calls to the number from another account will be routed upstream
* No number can transition from another state to `port_in`
* Any account can create a `port_in` number if the number doesn't yet exist in the system
* The public fields can be managed by the assigned account or its ancestors.
* `port_in` numbers can only transition to `in_service` (on first call) or `deleted` (if the port is canceled).

### Available

A number that is routing to the cluster but not yet assigned to an account.

* Any account can transition `available` numbers to `reserved` or `in_service` if the account is in good standing
* `available` numbers do not participate in number hunts

### Reserved

A number assigned to an account but not yet in use (be it a by a callflow, trunkstore, etc).

* `reserved` numbers do not participate in number hunts
* If the number is coming from the `discovery` state, the carrier module associated (`wnm_local`, `wnm_bandwidth`, etc) will attempt to acquire the number.
* `reserved` numbers can be put in service if requested by:
    * The requesting account is the same as the number's assigned account
    * The requesting account is an ancestor of the number's assigned account
    * The requesting account is a descendent of the number's assigned account
* Accounts can reserve a number if:
    * The requesting account is an ancestor of the number's assigned account
    * The requesting account is a descendent of the number's assigned account
* When a number is `reserved`, the new assignment is added to the number's history of assignments
* An account with the flag `pvt_wnm_allow_additions` set to true can create numbers into the `available` state. These numbers can then be `reserved` by the account or its sub-accounts.
* E911 and other features follow the number into `reserved`
* Public fields on the number can be managed by the assigned account or its ancestors

### In Service

A number assigned to an account and in use by an application such as callflows, trunkstore, etc.

* `in_service` numbers participate in number hunts
* Can transition to `reserved` by the assigned account or an ancestor
* E911 and other features follow the number into `in_service`
* Public fields on the number can be managed by the assigned account or its ancestors

## Released

A temporary state between `reserved` and `in_service` before placing the number back into the `available` pool (by default, see the `system_config/number_manager` doc for the `released_state` attribute to change which state the number transitions into).

* `released` does not participate in number hunts
* When a `reserved` or `in_service` number is released:
    * If the reserve history is empty, the number will either be disconnected, or disconnected and marked for deletion (controllabe with the `should_permanently_delete` flag in `system_config/number_manager`).
    * If the reserve history is not empty, the number is re-assigned to the previously assigned account
* E911 and other features are turned off

### Port Out

An internal state for releasing numbers. Works similarly to `port_in` except that inbound requests do not move to `in_service`.

### Disconnected

An internal state marking the number as archivable.

### Deleted

An internal state marking the number as hard-deletable. A crawler will walk the number databases and remove these entries after an aging period.

* Numbers in `deleted` are automatically purged from the database after an aging period
    * `system_config/number_manager.deleted_expiry_d`: How many days a number in the `deleted` state will remain before being deleted from the database.
