/*
Section: Crossbar
Title: Token restrictions
Language: en-US
*/

# Token restrictions
Module `cb_token_restrictions`.

## What is it?
Token restrictions - set of rules saved in auth token document. These rules grant access to API URIs.
**If the token document doesn't have any rules then this module won't apply any restrictions to request.**
These rules are created when the system creates an auth token.
Rules can be loaded from system template or account template.
System template located in `system_config/crossbar.token_restrictions`.
Account template located in `{ACCOUNT_DB}/token_restrictions`.

## How it works?
When you make request to Crossbar (API), the system loads rules from auth token (used for authentitcation) and tries to apply the rules to URI (`/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/`).
More information about URI structure can be found [here](basics.md).
If Crossbar doesn't find a match for all parameters (endpoint name, account id, endpoint arguments, HTTP method), then it halts the request and returns a 403 error.

## Template structure
Each template can have different rules for different authentication methods and user privelege levels.

```JSON
{
  "AUTH_METHOD_1": {
    "PRIV_LEVEL_1": {
      "RULES"
    },
    "PRIV_LEVEL_2": {
      "RULES"
    },
    ...
  },
  "AUTH_METHOD_2": {
    "PRIV_LEVEL_1": {
      "RULES"
    }
  },
  ...
}
```

* `AUTH_METHOD_#` - name of authentication method (`cb_api_auth`, `cb_user_auth`, etc) which created this auth token.
* `PRIV_LEVEL_#` - name of privilege level of authenticated user (`admin`, `user`, etc). This level is set in `priv_level` property of user document. If authentication method doesn't have a user associated (such as `cb_api_auth`) then select `admin` set of rules.
* `RULES` - set of rules which will be saved in auth token document.

Auth method and priv level can be matched with "catch all" term - `"_"`. If no exact match for auth method or priv level is found, the system will look for the 'catch all' rules, if any.
The rules are loaded into the auth token document when it is created (after successful authentication) and will be applied to any request using the auth token created.

Example template:
```JSON
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

## Rules structure (saved in token document)

```JSON
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

## Match order

### Endpoint match
At this step module compare resource from URI with resource names in token restrictions.
If URI is `/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/{MODIFIER}/` then endpoint will be `users`, and `{USER_ID}`, `{MODIFIER}` are arguments of this endpoint.
Rules applied to the last endpoint in URI.
You can use "catch all" (`"_"`) endpoint name. First tries exact endpoint name; if not found, try the catch-all (if it exists).

```JSON
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

### Account match

After Crossbar finds the endpoint it tries to find rules for the requested account.

```JSON
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
* `"_"` - match any account. **If the `"allowed_accounts"` parameter is missing, it is treated as a catch-all**

The first endpoint-rule object matched to the requested account will be used in the next step of argument matching.

### Endpoint arguments match

Endpoint argumnets matched with parameter `"rules"`.

```JSON
{
  "devices": [
    {
      "allowed_accounts": [
        "{ACCOUNT_ID}"
      ],
      "rules": {
        "/": [ ... ],
        "{DEVICE_ID}": [ ... ],
        "*": [ ... ]
      }
    }
  ]
}
```

The search is performed in the order in which they appear in the rules for first match. No more search after that.

`/` - match empty argument list:
**Matches**
* `/v2/accounts/{ACCOUNT_ID}/devices`

`*` - match any single, non-empty argument

**Matches**
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_1_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_2_ID}`
* etc

**Doesn't Match**
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync`

`#` - match any arguments (or no arguments)
**Matches**
* `/v2/accounts/{ACCOUNT_ID}/devices`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync`
* etc

`{DEVICE_ID}` - exact match
**Matches**
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`

**Doesn't Match**
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_1_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_2_ID}`
* etc

For matching more than one argument, you can use `/` to delineate how to process the arguments. You can mix and match special characters, explicit strings, etc.

`*/*/*` - match exactly three arguments
**Matches**
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{DID}`

`{DEVICE_ID}/#` - matches `{DEVICE_ID}` plus 0 or more arguments
**Matches**
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/sync`
* `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/quickcall/{DID}`
* etc

### HTTP method match
If endpoint matching fails to find a match, Crossbar will try to match the HTTP method used.

```JSON
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

## Manage accounts templates

### Get current account template

`GET /v2/accounts/{ACCOUNT_ID}/token_restrictions` return JSON like this:

```JSON
{
  "data": {
    "restrictions": {
      "{AUTH_METHOD}": {
        "{PRIV_LEVEL}": {
          "{API_ENDPOINT}": [
            {
              "allowed_accounts": {ACCOUNT_IDS},
              "rules": {
                "{ARGUMENT_MATCH}":{HTTP_METHODS}
              }
            }
          ]
        }
      }
    }
  }
}
```

If the account doesn't have token restrictions, the API will return a 404 error.

### Create/update account template

`POST /v2/accounts/{ACCOUNT_ID}/token_restrictions`

```JSON
{
  "data": {
    "restrictions": {
      "_": {
        "_": {
          "about": [
            {
              "allowed_accounts": [
                "_"
              ],
              "rules": {
                "/": [
                  "GET"
                ]
              }
            }
          ]
        }
      }
    }
  }
}
```

### Delete account template

`DELETE /v2/accounts/{ACCOUNT_ID}/token_restrictions`
