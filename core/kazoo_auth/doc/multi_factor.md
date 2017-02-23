### Kazoo Multi Factor Authentication

#### Overview

Integrating Kazoo to MFA providers to secure accessing to API endpoint. Users are authenticating with their usual Kazoo username and password and then will be authenticated through a second factors like Text message, a Phone call or push notification.

> **Note:** Please make sure that **NTP** service is configured on Kazoo servers and that your server's time is correct! Otherwise MFA request would be failed or incorect.

#### Multi Factor Aauthentication flow

MFA process involves these steps:

1. Initiate the authentication with your auth module/method with your user name and password (or token) as before.
2. If the auth method that you are using is configured to preforms the multi factor it goes to kazoo multi factor auth to process the request.
3. Based on auth module configuration, MFA provider settings will be fetched either from account's that have configured the auth module or from default provider in `system_auth`
4. If no MFA provider configuration was found, the auth request will be succesful and a token will be generated as a normal auth-request as before
5. If the MFA provider is disabled based on the configuration, a HTTP `401 unauthorized` error will be returned.
6. Provider module will try to validate the request and settings and return a HTTP `401 unauthorized` if something is invalid and provide an appropiate error message.
7. If the request doesn't have a response from MFA provider (if there is no `mfa_service_response` inside the body of the request) it will generate the required information that the client needs to preform second-factor auth with the provider. Crossbar will reply a HTTP `401 unauthorized` with the playlod of:
```json
{
    "data": {
        "messages": "client needs to preform second-factor authentication",
        "mfa_request": { //an json object which contains the neccessary information for performing the MFA with provider}
    }
}
```
8. After the client performed the MFA with the provider, it should issue the same auth request this time with paylod included the response from MFA provider
9. In this second request, kazoo provider module will try to verifying the request and provider response, if the provider response that user is claiming could be verfied the request will be authorized and a token will be generated as before. Otherwise a HTTP `401 unauthorized` error will be returned.

#### Where provider configuration comes from

If `mfa_option` is not present in the `Claims` or settings could not be fetched from Account's database the system default provider settings from `system_auth` will be used (which by default has an empty settings).
Otherwise the Account's settings will be used.

System config `auth/default_multi_factor_provider` contains the name of the default MFA provider for fetching the default provider settings.

#### Multi Factor Providers

##### Duo Two-Factor Authentication

Two-factor authentication with [Duo](https://duo.com/) makes it easy to add strong two-factor authentication to Kazoo. Users can recieve a text message, a phone call or push notification to their Duo mobile application.

###### Before you begin

1. [Sign up for a Duo account](https://signup.duo.com/).
2. Log in to the [Duo Admin Panel](https://admin.duosecurity.com/) and navigate to **Applications**.
3. Click **Protect an Application** and locate **Web SDK** in the applications list. Click **Protect this Application** to get your **Integration Key (IKey)**, **Secret Key (SKey)**, and **API Hostname**. (See [Duo's Getting Started](https://duo.com/docs/getting_started) for more help)
4. Generate a strong application key with a python code like below:

```python
import os, hashlib
print hashlib.sha1(os.urandom(32)).hexdigest()
```

###### Authenticate Users using Kazoo Duo integration

1. Use Crossbar multi factor API endpoint to configure Duo by you IKey, SKey, AKey, API hostname.
```json
{
    "data": {
            "provider_name": "duo",
            "provider_type": "multi_factor",
            "enabled": true,
            "settings": {
                "integration_key": "{YOUR_DUO_IKEY}",
                "secret_key": "{YOUR_DUO_SKEY}",
                "application_secret_key": "{YOUR_DUO_AKEY}",
                "api_hostname": "{YOUR_API_HOSTNAME}",
                "duo_expire": 300, //optional
                "app_expire": 3600 //optional
            }
    }
}
```
2. Enable multi factor (See [Crossbar Auth Config](../../../applications/crossbar/doc/multi_factor.md)) for a Crossbar auth module.
3. Users can now use a usual Crossbar authentication procedure (for example with `cb_user_auth`).
4. Kazoo auth MFA and Crossbar will return `siq_request` and `api_hostname` to client.
```json
{
    "data": {
        "messages": "client needs to preform second-factor authentication",
        "mfa_request": {
            "sig_request": "{SIQ_REQUEST}",
            "api_hostname": "{YOUR_DUO_API}"
        }
    }
}
```
5. Use `Duo-Web-v2.js` to implement Duo client side for performing 2FA authentication. See [Duo Web](https://duo.com/docs/duoweb) (steps 3 and beyond in **Instructions** section).
6. Send back the Duo response to kazoo by repeating the auth request in step 3 by adding `mfa_service_response` as payload.
```json
{
    "data": {
        "mfa_service_response": "{SIG_RESPONSE}",
    }
}
```
7. If `siq_response` is valid, user's will be authenticated and a token will be generated, otherwise a HTTP `401 unauthorized` will be returned.
