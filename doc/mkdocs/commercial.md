# Introduction

Kazoo gives you the tools to develop high-quality unified telecom applications. Crossbar REST API interface provides a simple way for external application to talk to Kazoo by making HTTP requests.

## Introduction to Crossbar REST APIS

Crossbar APIs provide access to resources via URL paths. To use a REST API, your application will make a HTTP request and parse the response.

Almost most of the time requests and responses for Crossbar endpoint are in JSON format, unless other format are expected for specific endpoint which you can find in the documentation.

Crossbar REST API is based on open standards like [JSON-Schema](http://json-schema.org/) (for data validation), you can use any web developer language to work with the API. Although for quick access to API tools like cURL (to work with HTTP protocol from terminal) and Postman (A GUI application to work with HTTP APIs) are come in handy.

A non-exhaustive list of all the things you can do with REST API:

* Access to your account and sub-accounts settings, add/remove and update any parameter.
* Retrieve a list of your account's devices, users and etc.
* Buy numbers, port number from other providers.
* Create callflows, assign numbers to users.
* Manage your office, defining Open Hour, After Hour, Menus, Voicemail boxes.

## Getting Started

After on-boarding is completed an account with default setting is setup and ready to use for you.

### Prerequisites

Before you begin you need to know the main URL path to the API which is usually provided to you by e-mail during on-boarding process. If you don't have this URL you can ask a your reseller salesperson to give you the URL.

Crossbar API requires the request to be authenticated, so you have to first get an authentication token before making any HTTP request to the API. The are various way to get this authentication token, you can learn more about them in [Authenticate your REST requests](applications/crossbar/doc/how_to_authenticate.md).

### Accessing to REST API resources

API resources are available at below location:

```
/{VERSION}/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

To learn about URL structure read [REST API Basics](applications/crossbar/doc/basics.md#basic-uri-structure).

But for now we assume we want to get out own account settings. For doing this we can simply use this cURL command:

```shell
$ curl -x GET \
     -H "X-Auth-Token: {AUTH_TOKEN}" \
     'http://crossbar_server.com:8000/v2/accounts/{ACCOUNT_ID}'
```

Here the explanation of the command:

* `-x GET` is telling cURL to perform a HTTP GET request.
* `-H "X-Auth-Token: {AUTH_TOKEN}"` is adding a HTTP header to the request. Here we add `X-Auth-Token` header required by Crossbar for authentication. `{AUTH_TOKEN}` is your authentication token.
* `http://...` part is telling cURL to which URL make the request.
* `crossbar_server.com/8000` is the main Crossbar API URL.
* `v2` is the API version.
* `accounts` is the name of the resource we want to have access.
* `{ACCOUNT_ID}` is a specific instance of this resource we want. Since we want to get our account information (we are using `accounts` resource here) we give our own account ID.

Running the command above will give us a response like this:

```json
{
  "data": {
    "timezone": "America/Los_Angeles",
    "reseller_id": "{RESELLER_ID}",
    "realm": "{ACCOUNT_REALM}",
    "name": "{ACCOUNT_NAME}",
    "language": "en-US",
    "is_reseller": false,
    "descendants_count": 0,
    "created": 63636183145,
    "caller_id": {
      "internal": {
        "name": "My Awesome Office"
      },
      "external": {
        "name": "My Awesome Office"
      },
      "emergency": {
        "name": "My Awesome Office"
      }
    },
    "blacklists": [],
    "available_apps": [],
    "id": "{ACCOUNT_ID}",
    "knm_allow_additions": false,
    "superduper_admin": false,
    "enabled": true
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{API_NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

Your response maybe be different from this example, since it depends on your account settings. All values in curly brackets (`{}`) are depends on your account settings and Crossbar API server and will be populated with the respective values.

## Next Steps

Learn more about Crossbar APIs:

* Read [REST API Basics](applications/crossbar/doc/basics.md) to know more about Crossbar REST API.
* How to [Authenticate your REST requests](applications/crossbar/doc/how_to_authenticate.md).
* Explore resources provided by REST APIs from this documentation.

Most of the resources are expected to have input (and some don't, read their documentation carefully to know how to work with them) which are explained in JSON-Schema form. In each resource documentation there is section explaining their schema.
