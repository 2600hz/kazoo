### Token Restrictions

#### About Token Restrictions

Token restrictions - set of rules saved in auth token document. These rules grant access to API URIs.

**If the token document doesn't have any rules then this module won't apply any restrictions to request.**

These rules are created when the system creates an auth token.
Rules can be loaded from system template or account template.
System template located in `system_config/crossbar.token_restrictions`.
Account template located in `{ACCOUNT_DB}/token_restrictions`.

#### How it works?

When you make request to Crossbar (API), the system loads rules from auth token (used for authentication) and tries to apply the rules to URI (`/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/`).
More information about URI structure can be found [here](basics.md).
If Crossbar doesn't find a match for all parameters (endpoint name, account id, endpoint arguments, HTTP method), then it halts the request and returns a 403 error.

#### Full Example

The following is a full example for reference. It is a rule for only allowing accounts to read/update their account configs (effectively barring them from deleting the account or creating sub-accounts.

```json
{
    "cb_user_auth": {
        "user":{
            "accounts":[
                {
                    "rules": {
                        "*": ["GET", "POST", "PATCH"]
                    }
                }
            ]
        }
    }
}
```

Place this on an account, as an admin, thusly:

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"restrictions":{"cb_user_auth": {"user":{"accounts":[{"rules": {"*": ["GET", "POST", "PATCH"]}}]}}}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

Now, when someone logs into this account (via user\_auth) and who's user\_doc is of type `user` (and not `admin`), they will be unable to create sub-accounts, and will receive an error response like this:

```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": {
    "cause": "access denied by token restrictions",
    "message": "forbidden"
  },
  "error": "403",
  "message": "forbidden",
  "node": "ghw5cA1GROHLEa43HETSKg",
  "request_id": "{REQUEST_ID}",
  "status": "error",
  "timestamp": "2017-06-07T23:16:25",
  "version": "4.0.0"
}
```

The following sections break down what's happening here.

#### Template structure

Each template can have different rules for different authentication methods and user privilege levels.

```json
{
  "AUTH_METHOD_1": {
    "PRIV_LEVEL_1": {
      "ENDPOINT_RULES"
    },
    "PRIV_LEVEL_2": {
      "ENDPOINT_RULES"
    },
    ...
  },
  "AUTH_METHOD_2": {
    "PRIV_LEVEL_1": {
      "ENDPOINT_RULES"
    }
  },
  ...
}
```

* `AUTH_METHOD_#` - name of authentication method (`cb_api_auth`, `cb_user_auth`, etc) which created this auth token.
* `PRIV_LEVEL_#` - name of privilege level of authenticated user (`admin`, `user`, etc). This level is set in `priv_level` property of user document. If authentication method doesn't have a user associated (such as `cb_api_auth`) then select `admin` set of rules.
* `ENDPOINT_RULES` - set of rules which will be saved in auth token document.

Auth method and priv level can be matched with "catch all" term - `"_"`. If no exact match for auth method or priv level is found, the system will look for the 'catch all' rules, if any.
The rules are loaded into the auth token document when it is created (after successful authentication) and will be applied to any request using the auth token created.

Example template:

```json
{
  "cb_user_auth": {
    "admin": {
      "RULES_FOR_ADMIN"
    },
    "user": {
      "RULES_FOR_USER"
    }
  },
  "_": {
    "admin": {
      "RULES_FOR_ADMIN"
    },
    "_": {
      "CATCH_ALL_RULES"
    }
  }
}
```

#### Rules structure (saved in token document)

```json
{
  "ENDPOINT_1": [
    {
      "allowed_accounts": [
        "ACCOUNT_ID_1",
        "ACCOUNT_ID_2"
      ],
      "rules": {
        "ARG_1": [
          "VERB_1",
          "VERB_2"
        ],
        "ARG_2": [
          "VERB_3"
        ]
        ...
      }
    },
    {
      "allowed_accounts": [
        "ACCOUNT_ID_3"
      ],
      "rules": {
        "ARG_1": [
          "VERB_1"
        ],
        ...
      }
    }
  ],
  "ENDPOINT_2": [
    {
      "rules": {
        "ARG_1": [
          "_"
        ]
      }
    }
  ],
  ...
}
```

* `ENDPOINT_#` - API endpoints (`"devices"`, `"users"`, `"callflows"`, etc)
* `ACCOUNT_ID_#` - any appropriate account ID
* `ARG_#` - arguments for endpoint separated by `/`
* `VERB_#` - any appropriate HTTP method (`"GET"`, `"PUT"`, etc)

#### Match order

##### Endpoint match

At this step module compare resource from URI with resource names in token restrictions.
If URI is `/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/{MODIFIER}/` then endpoint will be `users`, and `{USER_ID}`, `{MODIFIER}` are arguments of this endpoint.
Rules applied to the last endpoint in URI.
You can use "catch all" (`"_"`) endpoint name. First tries exact endpoint name: if not found, try the catch-all (if it exists).

```json
{
  "account": [
    { ... },
    { ... }
  ],
  "users": [
    { ... },
    { ... }
  ],
  "_": [
    { ... }
  ]
}
```

If a match is not found for the endpoint, this request is halted and a 403 error returned.
Each endpoint contains a list of objects with rules. Appropriate object is selected by `"allowed_account"` parameter.

##### Account match

After Crossbar finds the endpoint it tries to find rules for the requested account.

```json
{
  "devices": [
    {
      "allowed_accounts": [
        "{ACCOUNT_1_ID}",
        "{ACCOUNT_2_ID}",
        "{AUTH_ACCOUNT_ID}"
      ],
      "rules": {
        ...
      }
    },
    {
      "allowed_accounts": [
        "{DESCENDANT_ACCOUNT_ID}"
      ],
      "rules": {
        ...
      }
    }
  ]
}
```

List of account IDs set in parameter `"allowed_accounts"`. You can write exact IDs or one of the following special macros:

* `"{AUTH_ACCOUNT_ID}"` - match request account id to the account of the auth token
* `"{DESCENDANT_ACCOUNT_ID}"` - match any descendants of the auth account
* `"_"` - match any account. **If the `"allowed_accounts"` parameter is missing, it is treated as `"_"` (match any account).**

The first endpoint-rule object matched to the requested account will be used in the next step of argument matching.

##### Endpoint arguments match

Endpoint arguments matched with parameter `"rules"`.

```json
{
  "devices": [
    {
      "allowed_accounts": [
        "{ACCOUNT_ID}"
      ],
      "rules": {
        "/": [ ... ],
        "{DEVICE_ID}": [ ... ],
        "{DEVICE_ID}/sync": [ ... ],
        "*": [ ... ]
      }
    }
  ]
}
```

The search is performed in the order in which they appear in the rules for first match. No more search after that.

##### Rule keys

Key | Description
--- | -----------
`/` | match empty argument list (or used as separator between other keys)
`*` | match any single, non-empty argument
`#` | match any count of arguments (or zero arguments)
`string` | match exact string

**Examples:**

`/` - match empty argument list

**Matches**

* `/v2/accounts/{ACCOUNT_ID}/devices`

**Doesn't Match**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{DID}`

---

`*` - match any single, non-empty argument

**Matches**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_1_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_2_ID}`
* etc

**Doesn't Match**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync`

---

`#` - match any arguments (or no arguments)

**Matches**

* `/v2/accounts/{ACCOUNT_ID}/devices`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync`
* etc

---

`{DEVICE_ID}` - exact match

**Matches**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`

**Doesn't Match**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_1_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_2_ID}`
* etc

For matching more than one argument, you can use `/` to delineate how to process the arguments. You can mix and match special characters, explicit strings, etc.

---

`{DEVICE_ID}/quickcall/{DID}` - match exact list of arguments

**Matches**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{DID}`

**Doesn't Match**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{DID_2}`

---

`*/*/*` - match exactly three arguments

**Matches**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{DID}`

**Doesn't Match**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync`

---

`{DEVICE_ID}/#` - matches `{DEVICE_ID}` plus all arguments

**Matches**

* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{DID}`
* etc

##### HTTP method match

If endpoint matching fails to find a match, Crossbar will try to match the HTTP method used.

```json
{
  "devices": [
    {
      "allowed_accounts": [
        "{ACCOUNT_ID}"
      ],
      "rules": {
        "/": [
          "GET",
          "PUT"
        ],
        "{DEVICE_ID}": [
          "_"
        ],
        "#": [
          "GET"
        ]
      }
    }
  ]
}
```

List can contain any valid HTTP method ("GET", "PUT", "POST", "PATCH", "DELETE") or the "catch all" - `"_"`.

#### Schema

Schema for token restrictions



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].allowed_accounts.[]` |   | `string()` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].allowed_accounts` | Account allowed to match this item | `array(string())` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].rules./^[\w/#*]+$/.[]` |   | `string()` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].rules./^[\w/#*]+$/` | verbs | `array(string('GET' | 'PUT' | 'POST' | 'PATCH' | 'DELETE' | '_'))` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/.[].rules` | Rules applied to endpoint parameters | `object()` |   | `false` |  
`restrictions./^\w+$/./^\w+$/./^\w+$/` |   | `array(object())` |   | `false` |  
`restrictions./^\w+$/./^\w+$/` | Level of user privilege. '_' matches any priv level | `object()` |   | `false` |  
`restrictions./^\w+$/` | Name of authentication method used when creating token. '_' matches any auth method | `object()` |   | `false` |  
`restrictions` |   | `object()` |   | `false` |  



#### Fetch

> DELETE /v2/accounts/{ACCOUNT_ID}/token_restrictions

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

#### Fetch account's token restrictions

> GET /v2/accounts/{ACCOUNT_ID}/token_restrictions

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

#### Change account's token restrictions

> POST /v2/accounts/{ACCOUNT_ID}/token_restrictions

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d @data.txt
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/token_restrictions
```

File `data.txt` contains this restrictions:

* `admin` has full access
* `operator` can view/create/update devices (but not delete), full access to callflows, all other API restricted
* `accountant` can only view transactions, all other API restricted
* `user` can only view devices and other users. all other API restricted

```json
{
  "data": {
    "restrictions": {
      "_": {
        "admin": {
          "_": [
            {
              "rules": {
                "#": [
                  "_"
                ]
              }
            }
          ]
        },
        "operator": {
          "devices": {
            "rules": {
              "#": [
                "GET",
                "POST",
                "PUT"
              ]
            }
          },
          "callflows": {
            "rules": {
              "#": [
                "_"
              ]
            }
          },
          "_": {
            "rules": {
              "#": [
                "GET"
              ]
            }
          }
        },
        "accountant": {
          "transactions": {
            "rules": {
              "#": [
                "GET"
              ]
            }
          },
          "_": {
            "rules": {
              "#": []
            }
          }
        },
        "user": {
          "users": {
            "rules": {
              "#": [
                "GET"
              ]
            },
            "devices": {
              "rules": {
                "#": [
                  "GET"
                ]
              },
              "_": {
                "rules": {
                  "#": []
                }
              }
            }
          }
        }
      }
    }
  }
}
```
