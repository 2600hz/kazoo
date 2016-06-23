
### Give it a REST

Crossbar is the REST API server, from which developers can build applications that configure Kazoo's myriad services and functionalities.

#### Basic URI Structure

    /{VERSION}/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}

* {VERSION} - The version of the API you are calling.
    * v2 - Most APIs respond on the v2
    * v2 - A select number of APIs have newer behaviour. If you used the v2 version, it will work as before.
* {ACCOUNT\_ID} - Most requests operate against a specific account and thus require the account_id to route the resquest properly
* {RESOURCE\_ID} - When accessing a specific resource, like a device, user, or callflow, this is the {RESOURCE\_ID} points to the specific instance you're accessing.

##### Resources

There are two parts to how a request is routed in Crossbar: the REST endpoint and the resource ID. Let's break down a common URI and see how Crossbar figures out what is an endpoint and what is a resource ID.

Given a uri of `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`:

0. First, strip the version off the URI
    * Version: v2
    * URI Remaining: `/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`
1. See if the next token is a REST endpoint module. It is, so track the module for later routing:
    * Version: v2
    * Modules: `{accounts: []}`
    * URI Remaining: `/{ACCOUNT_ID}/devices/{DEVICE_ID}`
2. See if the next token is a REST endpoint module. It is not, so add the token to the last module's data:
    * Version: v2
    * Modules: `{accounts: [{ACCOUNT_ID}]}`
    * URI Remaining: `/devices/{DEVICE_ID}`
3. Repeat parsing. devices is a REST endpoint:
    * Version: v2
    * Modules: `{accounts: [{ACCOUNT_ID}], devices: []}`
    * Remaining URI: `/{DEVICE_ID}`
4. Repeat parsing. {DEVICE_ID} is an argument:
    * Version: `v2`
    * Modules: `{accounts: [{ACCOUNT_ID}], devices: [{DEVICE_ID}]}`

So we have a request to account {account\_id} to do something with a device {device\_id}.

##### HTTP Verbs

The HTTP verb will determine the class of actions to take against the resource. Generically speaking, the verbs map thusly:

* `/v2/accounts/{ACCOUNT_ID}/resources`
    * GET: Fetches a summary of configured resources
    * PUT: Creates a new instance of the resource.

* `/v2/accounts/{ACCOUNT_ID}/resources/{RESOURCE_ID}`
    * GET: Fetches the full representation of the resource
    * POST: Updates the full respresentation of the resource
    * DELETE: Deletes the resource

###### PATCH

Some resources are beginning to support the PATCH verb, allowing partial updates instead of requiring the request to include the full version of the document. `/users/{USER_ID}`, for instance, now supports PATCH:

    curl -v -X PATCH -H "Content-Type: application/json" -H "X-Auth-Token: {AUTH_TOKEN}" 'http://crossbar.server.com:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}' -d '{"data":{"vm_to_email_enabled":true}}'

This cURL request will patch the user's doc and set `vm_to_email_enabled` to `true`. All normal validation will occur after patching the document; this also means clients can PATCH documents with their own data only.

If a resource does not support PATCH yet, clients can expect to receive a `405 Method Not Allowed` error.

###### Tunneling the HTTP Verb

Some clients do not support the full range of HTTP verbs, and are typically limited to *GET* and *POST*. To access the functionalities of *PUT* and *DELETE*, you can tunnel the verb in a *POST* in a couple of ways:

1. As part of the [request envelope](#request_envelope): `{"data":{...},"verb":"PUT"}`
1. As a query string parameter: `/v2/accounts/{ACCOUNT_ID}/resources?verb=PUT`

###### Tunneling the Accept Header

Some clients do not support the ability to set the [Accept header](http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html) in the request, meaning they will not necessarily receive the response in the format they wish. Clients can append `accept=text/html` to the request body or query string to indicate they'd like the response processed as if the Accept header was `text/html`.

*Note*: `accept=csv` is retained for backwards-compatibility but it is encouraged to use a proper media type going forward.

##### Authentication Tokens

Most APIs require the client to have authenticated and received a token usable on subsequent requests. Crossbar provides a couple ways to receive an authentication token:

1. [User Authentication](./user_authentication)
2. [API Key Authentication](./api_authentication)

<a name="request_envelope"></a>
##### Request Envelope

When issuing a PUT or POST, a request body is needed. When submitting a JSON (the most common body), Crossbar expects a request envelope with a few bits of metadata:

* data: this top-level key will contain the object you wish to create/update
* auth_token: optionally put your auth token in the envelope
* verb: optionally tunnel a PUT or DELETE in a POST request

###### Sample Request Envelope

    {"data":{"foo":"bar"}
     ,"auth_token":"xyzpdq123bfg"
     ,"verb":"delete"
    }

##### Response Envelope

When receiving JSON responses, clients will receive the response in an envelope. The response includes some duplicated data from the HTTP Response headers, since some clients do not have access to those headers.

* data: contains the results of the request, if any
* auth\_token: contains the auth\_token used on the request
* status: One of 'success', 'error', or 'fatal'
* message: Optional message that should clarify what happened on the request
* error: Error code, if any
* request_id: ID of the request; usuable for debugging the server-side processing of the request

###### Sample Response Envelope

    {"data":{"the":"response", "data":"is here"}
     ,"auth_token":"xyzpdq123bfg"
     ,"status":"success"
     ,"request_id":"123abc456qwerty"
    }

##### Pagination

All listing APIs in v2 will be paginated by default (v1 will operate as before).

Let's take a look at the CDRs API to see how to interpret pagination.

###### CDR Pagination

We start with the typical CDR request for a listing of CDRs:

    curl -v -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://{SERVER_URL}:8000/v2/accounts/{ACCOUNT_ID}/cdrs
    {"auth_token": "{AUTH_TOKEN}"
     ,"data": [{CDR_OBJECT}
               ,{CDR_OBJECT}
               ,...
              ]
     ,"next_start_key": 63566193143
     ,"page_size": 25
     ,"request_id": "e8e7a793986ba86f15bd0c7b2ce91233"
     ,"revision": "bfcd0b7d8cbd647eaea262cb05be1b8b"
     ,"start_key": 63565345339
     ,"status": "success"
    }

The pagination response keys are `next_start_key`, `page_size`, and `start_key`.

* `next_start_key`: used to get the next page of results from this API. Will not exist if this is the last page.
* `start_key`: used to get back to this page of results (or start pagination from this point)
* `page_size`: the number of results returned in this page

Assuming no changes are made to the underlying documents, `start_key` will get you this page of results, and `next_start_key` will give you a pointer to the next page (imagine a linked-list).

###### Requesting a page

Using the `next_start_key` value, let's request the next page of CDRs:

    curl -v -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://{SERVER_URL}:8000/v2/accounts/{ACCOUNT_ID}/cdrs?start_key=63566193143
    {"auth_token": "{AUTH_TOKEN}"
     ,"data": [{CDR_OBJECT}
               ,{CDR_OBJECT}
               ,...
              ]
     ,"next_start_key": 63566542092
     ,"page_size": 25
     ,"request_id": "7256dc9201b6168305e883729b688d40"
     ,"revision": "627d3a28af809ad745c2fbfc8b7397a1"
     ,"start_key": 63566193143
     ,"status": "success"
    }

Observe now that `start_key` is the requested `start_key` and `next_start_key` points to the start of the next page of results.

&tip If `next_start_key` is missing from the response envelope, the response represents the last page of results.

You can also choose to receive pages in bigger or smaller increments by specifying `page_size` on the request. Do take care, as the `next_start_key` will probably vary if you use the same `start_key` but differing `page_size` values.

###### Disabling Pagination

If you want to disable pagination for a request, simply include `paginate=false` on the query string.
