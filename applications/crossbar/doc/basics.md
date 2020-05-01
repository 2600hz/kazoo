# API Basics

## Give it a REST

Crossbar is the REST API server, from which developers can build applications that configure Kazoo's myriad services and functionalities.

## Basic URI Structure

Requests Crossbar follows this structure:

```
/{VERSION}/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}
```

Here the explanation:

* `{VERSION}` - The version of the API you are calling. Currently the only support value is `v2`.
* `{ACCOUNT_ID}` - Most requests operate against a specific account and thus require the `account_id` to route the request properly
* `{RESOURCE_ID}` - When accessing a specific resource, like a device, user, or callflow, this is the `{RESOURCE_ID}` points to the specific instance you're accessing.

### Resources

There are two parts to how a request is routed in Crossbar: the REST endpoint and the resource ID. Let's break down a common URI and see how Crossbar figures out what is an endpoint and what is a resource ID.

Given a URI of `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`:

1. First, strip the version off the URI:
    * Version: `v2`
    * URI Remaining: `/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`
2. See if the next token is a REST endpoint module. It is, so track the module for later routing:
    * Version: `v2`
    * Modules: `{accounts: []}`
    * URI Remaining: `/{ACCOUNT_ID}/devices/{DEVICE_ID}`
3. See if the next token is a REST endpoint module. It is not, so add the token to the last module's data:
    * Version: `v2`
    * Modules: `{accounts: [{ACCOUNT_ID}]}`
    * URI Remaining: `/devices/{DEVICE_ID}`
4. Repeat parsing. devices is a REST endpoint:
    * Version: `v2`
    * Modules: `{accounts: [{ACCOUNT_ID}], devices: []}`
    * Remaining URI: `/{DEVICE_ID}`
5. Repeat parsing. {DEVICE_ID} is an argument:
    * Version: `v2`
    * Modules: `{accounts: [{ACCOUNT_ID}], devices: [{DEVICE_ID}]}`

So we have a request to account `{ACCOUNT_ID}` to do something with a device `{DEVICE_ID}`.

## HTTP Verbs

The HTTP verb will determine the class of actions to take against the resource. Generically speaking, the verbs map thusly:

* `/v2/accounts/{ACCOUNT_ID}/resources`
    * `GET`: Fetches a summary of configured resources
    * `PUT`: Creates a new instance of the resource.

* `/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}`
    * `GET`: Fetches the full representation of the resource
    * `POST`: Updates the full representation of the resource
    * `DELETE`: Deletes the resource

### PATCH

Some resources are supporting the `PATCH` verb, allowing partial updates instead of requiring the request to include the full version of the document. `/users/{USER_ID}`, for instance, supports `PATCH`:

```shell
curl -v -X PATCH \
   -H "Content-Type: application/json" \
   -H "X-Auth-Token: {AUTH_TOKEN}" \
   'http://crossbar.server.com:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}' \
   -d '{"data":{"vm_to_email_enabled":true}}'
```
This cURL request will patch the user's document and set `vm_to_email_enabled` to `true`. All normal validation will occur after patching the document; this also means clients can `PATCH` documents with their own data only.

If a resource does not support `PATCH` yet, clients can expect to receive a `405 Method Not Allowed` error.

### Tunneling the HTTP Verb

Some clients do not support the full range of HTTP verbs, and are typically limited to `GET` and `POST`. To access the functionalities of `PUT` and `DELETE`, you can tunnel the verb in a `POST` in a couple of ways:

1. As part of the [request envelope](#request-envelope): `{"data":{...}, "verb":"PUT"}`
2. As a query string parameter: `/v2/accounts/{ACCOUNT_ID}/resources?verb=PUT`

### Tunneling the Accept Header

Some clients do not support the ability to set the [Accept header](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) in the request, meaning they will not necessarily receive the response in the format they wish. Clients can append `accept=text/html` to the request body or query string to indicate they'd like the response processed as if the Accept header was `text/html`.

!!! note
    `accept=csv` is retained for backwards-compatibility but it is encouraged to use a proper media type going forward.

## Request Envelope

When issuing a `PUT`, `POST` or `PUT`, a request body is needed. When submitting a JSON (the most common body), Crossbar expects a request envelope with a few bits of metadata:

* `data`: this top-level key will contain the object you wish to create/update
* `auth_token`: optionally put your auth token in the envelope
* `verb`: optionally tunnel a `PUT` or `DELETE` in a `POST` request

**Sample Request Envelope:**

```json
{
    "data": {
        "foo": "bar"
    },
    "auth_token": "{AUTH_TOKEN}",
    "verb": "delete"
}
```

## Request Data

When using `PATCH` to edit entities, if you want to remove a field from the entity, set it to `null`:

```json
{
    "data": {
        "update":"this",
        "exists": null
    }
}
```

This request would set `update` to `this` and would remove `exists` from the entity.

## Response Envelope

When receiving JSON responses, clients will receive the response in an envelope. The response includes some duplicated data from the HTTP Response headers, since some clients do not have access to those headers.

* `data`: contains the results of the request, if any
* `auth_token`: contains the `auth_token` used on the request
* `status`: One of `success`, `error`, or `fatal`
* `message`: Optional message that should clarify what happened on the request
* `error`: Error code, if any
* `request_id`: ID of the request; usable for debugging the server-side processing of the request

**Sample Response Envelope:**

```json
{
    "data": {
        "the": "response",
        "data": "is here"
    },
    "auth_token": "{AUTH_TOKEN}",
    "status": "success",
    "request_id": "{REQUEST_ID}"
}
```

## Pagination

All listing APIs will be paginated by default.

Let's take a look at the CDRs API to see how to interpret pagination.

### CDR Pagination

We start with the typical CDR request for a listing of CDRs:

```shell
curl -v \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER_URL}:8000/v2/accounts/{ACCOUNT_ID}/cdrs
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {CDR_OBJECT},
        {CDR_OBJECT},
        ...
    ],
    "next_start_key": "g2wAAAACbgUAvn1W1A5tAAAACDk4MDE2ODAwag",
    "page_size": 25,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": "g2wAAAACbgUAb0ZX1A5oAWpq",
    "status": "success"
}
```

The pagination response keys are `next_start_key`, `page_size`, and `start_key`.

* `next_start_key`: used to get the next page of results from this API. Will not exist if this is the last page.
* `start_key`: used to get back to this page of results (or start pagination from this point)
* `page_size`: the number of results returned in this page

Assuming no changes are made to the underlying documents, `start_key` will get you this page of results, and `next_start_key` will give you a pointer to the next page (imagine a linked-list).

### Encoded Start Keys (Kazoo 4.2+ Only)

As you can see from the response above, both the `start_key` and `next_start_key` are encoded as URL-safe Base64 strings of their Erlang term representation. A couple character substitutions (`_` for `/` and `_` for `+`) and one character removal (`=`) ensures a string that plays nice in URLs.

In practice, the client should treat these keys as opaque and supply them as-is in future requests.

### Requesting next page

Using the `next_start_key` value, let's request the next page of CDRs:

```shell
curl -v \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER_URL}:8000/v2/accounts/{ACCOUNT_ID}/cdrs?start_key=g2wAAAACbgUAb0ZX1A5oAWpq
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {CDR_OBJECT},
        {CDR_OBJECT},
        ...
    ],
    "next_start_key": "g2wAAAACbgUAbyYO1A5tAAAACDYwMTIzYjdiag",
    "page_size": 25,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": "g2wAAAACbgUAb0ZX1A5oAWpq",
    "status": "success"
}
```

Observe now that `start_key` is the requested `start_key` and `next_start_key` points to the start of the next page of results.

!!! note
    If `next_start_key` is missing from the response envelope, the response represents the last page of results.

You can also choose to receive pages in bigger or smaller increments by specifying `page_size` on the request. Do take care, as the `next_start_key` will probably vary if you use the same `start_key` but differing `page_size` values.

### Setting Page Size

By default, API requests have a page size of 50 results. This value is customizable by system administrator in the `crossbar.maximum_range` system config setting.

For individual API request, you can also include a `page_size` query string parameter. For example: `http://{SERVER}:8000/v2/{API_URL}?page_size=25`.

### Setting sorting order

By default, Crossbar returns the results in descending order. To get results in ascending order either set `ascending=true` (Kazoo 4.2+ only) or `descending=false` in the request query string.

!!! note
    The field used to sort the individual API results depends on the internal implementation of the API endpoint and is not controllable by the client.

### Disabling Pagination

If you want to disable pagination for a request, simply include `paginate=false` on the query string.

#### Protecting from (un)intentional abuse

Since pagination can be turned off by a client-supplied query string parameter, it is important that KAZOO still protect itself from overly large datasets being loaded. Examples seen include large CDR listings, call recording listings, and ledger listings.

Therefore, during a non-paginated request, KAZOO monitors memory consumption of the handling server process and will abort the request if the processing is exceeding a high watermark setting (configured by the system operator). The client can expect to receive an HTTP "416 Range Not Satisfiable" error as a result of exceeding the limit.

The memory limit threshold can be set by system administrators with

    sup kapps_config set_default crossbar request_memory_limit 10485760

In this case, memory per-request (for listings) is constrained to 10MB.

## Chunked Response

Starting with Kazoo 4.2, most of the summary API endpoints can send [chunked responses](https://en.wikipedia.org/wiki/Chunked_transfer_encoding). Some known APIs, which tend to have larger datasets, are chunked by default (e.g. `/cdrs/interaction` and `/ledgers/{LEDGER}`).

The query string parameter `is_chunked` (boolean value) can be used to enable or disable chunking per-request.

To set the default chunk size, you can use `chunk_size` in the query string. Default value is `50`.

## Pretty Printing

If needed, the JSON response to be pretty printed, the server can can do so.

Include pretty printing inside the header.

```shell
curl -v \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -H "X-Pretty-Print:true" \
    http://{SERVER_URL}:8000/v2/accounts/{ACCOUNT_ID}/
```

If the client cannot use headers the options can be included inside the URI.

```shell
curl -v \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    http://{SERVER_URL}:8000/v2/accounts/{ACCOUNT_ID}?pretty_print=true
```

## Requesting a range of binary data

It is useful to be able to get just a section of a file when streaming or
resuming a download. This can be accomplished with the range header, e.g.:

```shell
curl -v \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -H "Accept: audio/mpeg" \
    -H "Range: bytes={START_BYTE}-{END_BYTE}" \
    http://{SERVER_URL}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MESSAGE_ID}/raw
```

## Requesting data in CSV format

In some cases (e.g. CDR) its possible to request data in CSV format You must define the Content-type in the header you can define the file name in the request header or URL (Optional)

```shell
curl -v -X GET \
    -H "Accept: text/csv" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "X-File-Name: {FILE_NAME}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs
```

or

```shell
curl -v -X GET \
    -H "Accept: text/csv" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/cdrs?file_name={FILE_NAME}
```

## Timestamps

KAZOO, unless explicitly stated, represents time in Gregorian seconds.

Conversion with UNIX timestamps is straightforward:

```
UnixEpochInGregorian = 62167219200

gregorian_to_unix($greg) -> $greg - 62167219200

unix_to_gregorian($unix) -> $unix + 62167219200
```
