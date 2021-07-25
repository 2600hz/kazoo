
# Overview

Teletype listens for events within Kazoo and notifies those that are interested. This application replaces the `notify` application of old.

## Autostart modules

The teletype modules listed in the `autoload_modules` section of `system_config/notify` will
automatically be started when teletype is started.

## Emergency Call Notification

The `emergency_bridge` notification is triggered when stepswitch recognizes an emergency call is being attempted.

Teletype will attempt to fetch the `emergency_notfication_contact_emails` parameter from the which is set on a phone number's `e911.notification_contact_emails`, but will fallback to sending it to the Admins if this parameter has not been explictly set.

### Hotdesking

Stepswitch attempts to identify as much data as possible before publishing an `emergency_bridge`, including resolving the `owner_id` (if present) of a device to a username and email address.  If a device does not have an owner, only the device name and id will be resolved.

Hotdesking allows for multiple users to be hotdesked to a device simultaneously.

When a device has a single hotdesked user, that user will be resolved as the owner and their metadata published in the notification.

However, if multiple users are actively hotdesked into a device, a specific owner cannot be determined, so the device is treated as a raw device and user metadata is not published in the notification.
