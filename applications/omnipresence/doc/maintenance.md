
## Current Subscriptions

List the current subscriptions tracked by:

* Across the omnipresence app:

        sup omnipresence_maintenance current_subscriptions

* Across a SIP Realm:

        sup omnipresence_maintenance current_subscriptions SipRealm

* For a specific SIP Username:

        sup omnipresence_maintenance current_subscriptions SipRealm SipUsername


## Subscribe

Send a test subscription and receive the current state (subscription will time out after 1 second):

    sup omnipresence_maintenance subscribe SipRealm SipUsername

## Send MWI Update

Update the MWI for a SIP device:

    sup omnipresence_maintenance send_mwi_update username@realm.com NewMessages WaitingMessages

* `NewMessages`: integer
* `WaitingMessages`: integer

## List Cached Terminated Call IDs

Omnipresence caches call-ids when they terminate, to avoid publishing a delayed ringing/answered event and confusing all the things. This function allows an admin to view the currently cached Call IDs:

    sup omnipresence_maintenance list_terminated_callids`
