### Push Notification Subscriptions

#### About Push Notification Subscriptions

#### Schema

Schema for a push notification subscription



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`notification_preferences.[]` |   | `string()` |   | `true`
`notification_preferences` | The types of notifications that the user wants to receive | `array(string('new_voicemail' | 'missed_call'))` |   | `true`
`notification_registration_id` | The registration id/device token for the mobile device to receive notifications | `string()` |   | `true`
`notification_type` | The type of notification service to use | `string('apns' | 'fcm')` |   | `true`



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

