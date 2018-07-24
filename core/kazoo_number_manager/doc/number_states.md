Numbers travel through a variety of states within Kazoo during their lifecycle.

Let's enumerate those states and then see how they fit together!

## Number States

### `discovery`

An internal state used to temporarily hold numbers that are available for users to acquire from the system.

* Populated by API requests to activate numbers for an account that don't yet exist in the system
* Numbers in `discovery` do not participate in number hunting
* No number can transition from another state to `discovery`
* There are no public fields for `discovery` numbers
* Numbers in `discovery` are automatically purged from the database after a configurable time
    * `system_config/tasks.discovery_expiry_d`: How many days a number in the `discovery` state will remain before being deleted from the database.

### `port_in`

When an account initiates a port request via the APIs, the number is created in the `port_in` state.

* Numbers in `port_in` will automatically transition to `in_service` the first time a call is received from a carrier to that number (the number hunt process)
* Only used for internal number hunting within an account; calls to the number from another account will be routed upstream
* No number can transition from another state to `port_in`
* Any account can create a `port_in` number if the number does not yet exist in the system
* The public fields can be managed by the assigned account or its ancestors.
* `port_in` numbers can only transition to `in_service` (on first call) or `deleted` (if the port is canceled).

### `available`

A number that is routing to the cluster but not yet assigned to an account.

* Any account can transition `available` numbers to `reserved` or `in_service` if the account is in good standing
* `available` numbers participate in number hunts
* Non-local numbers can be released into the `available` state but they have to go through an `aging` period first
* Admin accounts are allowed to create `available` numbers if their `pvt_wnm_allow_additions` flag is set to true

### `reserved`

A number assigned to an account but not yet in use (be it a by a callflow, trunkstore, etc).

* `reserved` numbers do not participate in number hunts
* If the number is coming from the `discovery` state, the carrier module associated (`knm_local`, `knm_bandwidth`, etc) will attempt to acquire the number.
* `reserved` numbers can be put `in_service` when:
    * The requesting account is the same as the number's assigned account
    * The requesting account is an ancestor of the number's assigned account
    * The requesting account is a descendent of the number's assigned account
* Accounts can reserve a number when:
    * The requesting account is an ancestor of the number's assigned account
    * The requesting account is a descendent of the number's assigned account
* When a number is `reserved`, the new assignment is added to the number's history of assignments
* `available` numbers can be transitioned into the `reserved` state
* E911 and other features follow the number into `reserved` but cannot be edited until the number is `in_service`
* Public fields on the number can be managed by the assigned account or its ancestors

### `in_service`

A number assigned to an account and in use by an application such as callflows, trunkstore, etc.

* `in_service` numbers participate in number hunts
* Can transition to `reserved` by the assigned account or an ancestor
* E911 and other features follow the number into `in_service`
* Public fields on the number can be managed by the assigned account or its ancestors

## `released`

A temporary state between `reserved` and `in_service` before placing the number back into the `available` pool.
Deactivated by default: `system_config/number_manager.released_state` defaults to `available`.

* `released` does not participate in number hunts
* Releasing a `reserved` or `in_service` number,
    * when the reserve history is empty: the number will be disconnected then either returned or marked for deletion (see `system_config/number_manager.should_permanently_delete`)
        * Note: `knm_local` and `knm_mdn` numbers always go to deletion regardless of this config flag's value
    * when the reserve history is not empty, the number is assigned to the previous account in the history
* E911 and all other number features are stripped off the number

### `port_out`

An internal state for releasing numbers.
Works similarly to `port_in` except that inbound requests do not move to `in_service`.

### `deleted`

An old internal state marking the number as hard-deletable.
This state is no longer used: numbers are no longer soft-deleted.
