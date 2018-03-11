### Push Notification Subscriptions

#### About Push Notification Subscriptions

Push notification subscriptions allows users to specify events that they want to receive push notifications about.
A push notification subscription document describes the user's device, the application they want to receive notifications through and the events they want to receive notifications about.
Once the document is created, when the events specified in `notification_preferences` occur, they will receive a push notification from the given app on the given mobile device.
If a user wants to have multiple devices, they will need a push notification subscription for each.

#### Schema

Schema for a push notification subscription



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`notification_preferences.[]` |   | `string('new_voicemail' | 'chat')` |   | `true` |  
`notification_preferences` | The types of notifications that the user wants to receive | `array(string('new_voicemail' | 'chat'))` | `["new_voicemail", "chat"]` | `true` |  
`notification_registration_id` | The registration id for the mobile device | `string()` |   | `true` |  
`notification_type` | The type of notification service to use | `string('apns' | 'fcm')` |   | `true` |  



#### Fetch
Gets the push notification registration for the supplied device for the supplied app

> GET /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{MOBILE_DEVICE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{MOBILE_DEVICE_ID}
```

#### Create
Creates a push notification registration for the supplied device for the supplied app

> PUT /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{MOBILE_DEVICE_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{MOBILE_DEVICE_ID}
```

#### Change
Updates a push notification registration for the supplied device for the supplied app. Good for changing the notification preferences
or updating the device token.

> POST /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{MOBILE_DEVICE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{MOBILE_DEVICE_ID}
```

#### Remove
Deletes a push notification registration for the supplied device for the supplied app.

> DELETE /v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{MOBILE_DEVICE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/push_notification_subscriptions/{APP}/{MOBILE_DEVICE_ID}
```

