# Stepswitch Emergency Call Handling

## Emergency Call Identification

When stepswitch attempts to execute a bridge of a call, it first checks to see if the call is flagged as an emergency call.  If this condition is met, it will publish an `emergency_bridge` message with metadata for the call.

### Emergency Test Calls

You can also define [test call rules](rules.md) for the regional emergency services test number.

Stepswitch will attempt to match the dialed DID of an emergency call against the collected resource endpoints `rules_test` object in an effort to identify test emergency calls.

## Emergency Call Notification

If stepswitch identifies an emergency route it immediately publishes the `emergency_bridge` AMQP message in `kapi_notfications`.  This message is consumed by the `teletype_emergency_bridge`  module as well as configured `notifications` or `emergency_bridge` specific webhooks.

## Hotdesking

Stepswitch attempts to identify as much data as possible before publishing an `emergency_bridge`, including resolving the `owner_id` (if present) of a device to a username and email address.  If a device does not have an owner, only the device name and id will be resolved.

Hotdesking allows for multiple users to be hotdesked to a device simultaneously.

When a device has a single hotdesked user, that user will be resolved as the owner and their metadata published in the notification.

However, if multiple users are actively hotdesked into a device, a specific owner cannot be determined, so the device is treated as a raw device and user metadata is not published in the notification.
