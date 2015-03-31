/*
Section: Whistle Number Manager
Title: Number States
Language: en-US
Version: 3.20
*/

Numbers travel through a variety of states within Kazoo during their lifecycle.

Let's first enumerate those states and then see how they fit together!

## Number States

### Discovery

An internal number state used to temporarily hold numbers that are available for users to acquire from the system.

* Populated by API requests to activate numbers for an account that don't yet exist in the system
* Numbers in `discovery` do not participate in number hunting
* No number can transition from another state to `discovery`
* There are no public fields for `discovery` numbers

### Port In

When an account initiates a port request via the APIs, the number is created in the `port_in` state.

* Numbers in `port_in` will automatically transition to `in_service` the first time a call is received from a carrier to that number (the number hunt process)
* Only used for internal number hunting within an account; calls to the number from another account will be routed upstream
* No number can transition from another state to `port_in`
* Any account can create a `port_in` number if the number doesn't yet exist in the system
* The public fields can be managed by the assigned account or its ancestors.
* `port_in` numbers can only transition to `in_service` (on first call) or `deleted` (if the port is canceled).
