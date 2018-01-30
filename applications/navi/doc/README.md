# Overview
The Navi application is used for listening to notifications through amqp and using them to create push notifications for iOS and Android devices.

## Architectural Overview
Navi functionality is implemented across several modules:
* cb\_push\_notification\_subscriptions
* navi\_listener
* navi\_module\_sup
* navi\_notification\_server\_sup
* nv\_apns
* nv\_fcm

### cb\_push\_notification\_subscriptions.erl
This is the crossbar module where users can register for push notifications. See the schema for a push notification subscription for the necessary data. A couple fields of note:
* app\_name: this field determines that app that will deliver the notification to the user. There should be a corresponding app in the system\_config.navi 
document with a certificate and key (for iOS apps) or an api-key (for Android apps).
* notification\_registration\_id: this is the device token for the device to receive the push notification. It must be sent from the app that will deliver the
push notification to kazoo via the crossbar module.

These documents will be used by navi\_listener to determine which device(s) a user wants to receive push notifications on.

### navi\_listener.erl
This module is responsible for listening to events on amqp and extracting the data included in the corresponding push notification. To listen to an event
and create a notification for it you need to do three things:
1. Add the event to the BINDINGS list
2. Create a handler function which processes the event and collates the data for the notification.
3. Use the cb\_push\_notification\_subscriptions documents to determine the device to send the notification to.
4. Call navi\_module\_sup:push/4, which will begin the process of sending the push notification (Encapsulated in navi\_listener:do\_notifications/3)
	* do\_notifications takes three arguments. 
		* A list of push\_notification\_subscription documents representing the registrations for each device that should receive this notification
		* The message to appear in the body of the push notification
		* A proplist containing any extra parameters you want to be sent in the notification (You may need to modify nv_apns and nv_fcm so it formats your custom data in the notification payload how you want).

### navi\_module\_sup.erl
This module is the supervisor for most of the activity in navi. On startup, it will create a navi\_notification\_server\_sup for each app in the system\_config.navi document, which will in turn start the
process that actually sends the push notifications. It is also the interface for sending notifications. To send a push notification, simply call:
``navi_module_sup:push(RegistrationId, AppName, NotificationType, Message, ExtraParameters).``
where:
* RegistrationId is the device token of the device to send notifications to
* AppName is the name of the app to deliver the notification (as defined in the system\_config.navi document)
* NotificationType is either <<"apns">> or <<"fcm">>, denoting which kind of notification server to use
* Message is the text you want to appear in the body of the notification
* ExtraParameters contains any sort of metadata you want to pass to the receiving application such as badge count, subtopics or other data specific to the app

### navi\_notification\_server\_sup
This module superverises one individual notification server and passes the push notification request from navi\_module\_sup to the actual notification server.
Many instances of the module are spawned by navi\_module\_sup with arguments defining which app the notification server is for. As long as system\_config.navi has all the apps
that you want in it, you will not need to interface with this module.

### nv\_apns
This module contains the logic for sending push notifications to iOS apps. You will not need to interact with this module if everything is set up properly unless you want to add
custom data to your notifications.

### nv\_fcm
This module contains the logic for sending push notifications to Android apps. You will not need to interact with this module if everything is set up properly unless you want to add
custom data to your notifications.

## Adding Apps
The navi\_maintenance module exports two functions that allow you to easily add apps to system\_config.navi through a sup command.
*To add an iOS app:*
``sup navi_maintenance add_apns_app <app_name> <topic> <environment> <path to certificate file> <path to key file>``
where:
* app\_name is an identifier for the app
* topic is the "app bundle" or "app ID" which you can get from xcode (usually of the form com.company.app-name
* environment is either "prod" or "dev" (quotations excluded), you must make sure you use the corresponding certificate and key for the environment.
* the path to the certificate and key files must be absolute paths, and the files must be in .pem format. Follow the instructions in the Apple developer portal for
getting a certificate for either development or production push notifications.

Additionally the app will need to be configured to allow push notifications in its capabilities. Follow the Apple documentation for adding push notifications to your app.

*To add an Android app:*
``sup navi_maintenance add_fcm_app <app_name> <api-key>``
where:
* app\_name is the same as above
* api-key is the push notification api-key for the app you want to add

All apps that you add in this way should automatically have push notification servers started for them by navi

## Acknowledgments
* The fcm push notification logic comes from a slightly adapted version of fcm-erlang which can be found at: https://github.com/softwarejoint/fcm-erlang
* The apns push notification logic comes from apns4erl which can be found at: https://github.com/inaka/apns4erl

Hey, listen!
