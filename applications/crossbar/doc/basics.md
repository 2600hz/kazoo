# The KAZOO API Primer

KAZOO’s REST API is big. We have over 100 supported endpoints, all that can be used to create, retrieve, update, or delete resources. That’s a lot of CRUD!

All of these endpoints work to provide you with a robust toolset for building applications, programmatically controlling your KAZOO accounts, or otherwise retrieving telephony data from KAZOO. This article lists information that you may find helpful in making your first API requests on the platform.

## Using the API

---

NOTE: The documentation assumes you own a KAZOO account, whether in one of our commercial environments or your own open source installation. Visit our website to learn about getting started.

KAZOO runs an API server that we call **Crossbar**. By default, Crossbar listens for incoming HTTP requests on port 8000. If you're experienced with making HTTP requests to an API, Crossbar should be pretty easy to get started with. Generally, the biggest hurdles for people getting started are that they have the incorrect server URL, the wrong account ID, or that they are not correctly formatting things such as their account ID when constructing auth tokens or hashes.

## How Do I Authenticate Crossbar Requests?

---

The most convenient way to make API requests with KAZOO across different interfaces like cURL, SDKs, or Postman, is by generating an access token. Once this is complete, authenticating requests is as easy as adding the `X-AUTH-TOKEN` HTTP header to your requests. Most KAZOO SDKs will allow you to create a persistent client that takes in a token in order to instantiate.


DID YOU KNOW?: You can obtain an access token by logging into your KAZOO’s MonsterUI portal. When you’re logged in, pressing the ‘**d**’ key will bring up debug fields. One of them is a valid access token for your account. It’s the one your MonsterUI is using to make requests to KAZOO!

You can also obtain an access token by making a request to the API using [HTTP Basic Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#basic_authentication_scheme), authenticated with your Account ID and Password, to first get your API key. Once you have an API key, you can use it to get the access token. For example, if you were to use cURL, you could do the following.

---

Using the endpoint `/v2/accounts/{$ACCOUNT_ID}/api_key` and HTTP Basic Authorization:

**Request**

```bash
curl -v -X GET \
    --basic --user $ACCOUNT_ID:$PASSWORD \
    http://$SERVER:8000/v2/accounts/{ACCOUNT_ID}/api_key
```

**Response**

```jsx
{
   "data":{
      "api_key":"{API_KEY}"
   },
   "revision":"{REQUEST_ID}",
   "request_id":"{REQUEST_ID}",
   "status":"success"
}
```

---

Once you’ve fetched your API key, you can then pass it as a request body to the `/v2/api_auth` endpoint. The response will include an auth token you can then use for future requests.

**Request**

```bash
curl -v -X PUT \
    -d '{"data": {"api_key":"{API_KEY}"} }' \
    http://$SERVER:8000/v2/api_auth
```

**Response**

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "account_id": "{ACCOUNT_ID}",
        "apps": [...],
        "is_reseller": true,
        "language": "en-US",
        "owner_id": "{OWNER_ID}",
        "reseller_id": "{RESELLER_ID}",
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

Make sure that you are constructing the data envelope correctly when sending the request. 

---

If you already have a valid auth token, there is no need to do the first request with HTTP basic authentication. You can just use the auth token for all requests like this:

```bash
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://$SERVER:8000/v2/accounts/{ACCOUNT_ID}
```

---

Once obtained, the auth token can be used to make API requests scoped to your account. Be aware that auth tokens only last a default of one hour. 

This is pretty much all you need to know to be authenticated to make requests! You can see all authentication APIs and associated payloads in the API reference.

## An Example API Request

---

Now that we have an auth token, let’s make a request to fetch data from a KAZOO account. Let’s try sending a `GET` request a simple (but important) endpoint `/v2/accounts/{account_id}`. Don’t forget to include the authorization token as a header, with a key of `X-Auth-Token`.

**Request**

```jsx
$ curl -x GET \
		-H "X-Auth-Token: {AUTH_TOKEN}" \
		'http://{server}:8000/v2/accounts/{ACCOUNT_ID}'
```

**Response**

```jsx
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "billing_mode": "manual",
        "call_restriction": {},
        "caller_id": {},
        "created": 63621662701,
        "dial_plan": {},
        "enabled": true,
        "id": "{ACCOUNT_ID}",
        "is_reseller": false,
        "language": "en-us",
        "music_on_hold": {},
        "name": "child account",
        "preflow": {},
        "realm": "aeac33.sip.2600hz.com",
        "reseller_id": "undefined",
        "ringtones": {},
        "superduper_admin": false,
        "timezone": "America/Los_Angeles",
        "wnm_allow_additions": false
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

As you can see, this account endpoint will contain configurations related to your account. You can get more information about this endpoint from the Accounts documentation section.

## KAZOO URI Structure

---

When you make a request to KAZOO, the majority of requests you’ll want to make act upon a couple of important identifiers.

Let’s break down the following URI structure:

`/{VERSION}/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}`

The components of this URI are:

1. `{VERSION}`
    - The version of the API endpoint, currently `v2` for all supported APIs found in this documentation.

1. `/accounts/{ACCOUNT_ID}`
    - This is your KAZOO account ID, a 32-character hex-encoded identifier.

1. `/{RESOURCE}/{RESOURCE_ID}`
    - A resource, such as `devices`, `users`, `callflows`, et cetera.
    - The ID of the resource, usually also hex-encoded identifier.

## Helpful Information About KAZOO’s API

---

This section lists additional information that you may find helpful about navigating KAZOO’s numerous APIs.

### HTTP Verbs

It’s helpful to identify how the HTTP request verb defines what we want to *do* when we request a resource. There are a couple URI patterns that you’ll see throughout the API, with verb patterns that can be described generically like this:

- `/v2/accounts/{ACCOUNT_ID}/resources`
    - Example: `/v2/accounts/{ACCOUNT_ID}/devices`
    - `GET`: Fetches a summary of configured resources. A GET will return a collection of devices under the account.
    - `PUT`: Creates a new instance of the resource. Sent with JSON, your request body will contain the necessary information about the resource you want to create.
- `/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}`
    - Example: `/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}`
    - `GET`: Fetches the full representation (all exposed fields) of a resource.
    - `POST`: Updates the full representation of the resource.
    - `DELETE`: Deletes the resource.

### The Request Envelope

When issuing a `PUT`, `POST` or `PUT`, a request body is needed. When submitting a JSON (the most commonly accepted format across endpoints), Crossbar expects a request envelope with a few bits of metadata:

- `data`: This top-level key will contain the object you wish to create/update
- `auth_token`: Optionally, you can include your auth token in the envelope
- `verb`: Optionally tunnel a `PUT` or `DELETE` in a `POST` request

**Sample Request Envelope:**

```json
{
    "data": {
        "foo": "bar"
    },
    "auth_token": "{AUTH_TOKEN}",
    "verb": "DELETE"
}
```

### Information About PATCH

Some resources support the `PATCH` verb, which allows partial updates instead of requiring the request to include the full version of the document. `/users/{USER_ID}`, for instance, supports `PATCH`:

```bash
curl -v -X PATCH \
   -H "Content-Type: application/json" \
   -H "X-Auth-Token: {AUTH_TOKEN}" \
   'http://crossbar.server.com:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}' \
   -d '{"data":{"vm_to_email_enabled":true}}'
```

This cURL request will patch the user's document and set `vm_to_email_enabled` to `true`. All normal validation will occur after patching the document; this also means clients can PATCH documents with their own data only.

If a resource does not support `PATCH` yet, clients can expect to receive a `405 Method Not Allowed` error.