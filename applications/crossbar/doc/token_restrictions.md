/*
Section: Crossbar
Title: Token restrictions
Language: en-US
*/

# Token restrictions
Module `cb_token_restrictions`.

## What is it?
Token restrictions - set of rules saved in auth token document. This rules grant access to API URI-s.
**If token document dont have any set of rules - then this module dont apply any restrictions to request.**
This rules created when the system create auth token.
Rules can be loaded from system template or account template.
System template located in `system_config/crossbar.token_restrictions`.
Account template located in `{AccountDB}/token_restrictions`.

## How it works?
When you make request to Crossbar (API), the system load rules from token, which used for authentitcation, and try apply this rules to URI (`/v1/accounts/1234...xyz/devices/123...xyz/`).
More information about URI structure you can find [here](basics.md).
If system not found match for all parameters (endpoint name, account id, endpoint arguments, HTTP method), then it halt this request and return 403 error.

## Template structure
Each template can have differnet rules for different authentication method and user privelege level.
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
`AUTH_METHOD_#` - name of used authentication method (`cb_api_auth`, `cb_user_auth`, etc) which created this auth token.
`PRIV_LEVEL_#` - name of privilege level of authenticated user (`admin`, `user`, etc). This level set in `priv_level` property of user document. If authentication method dont have usern id (such as `cb_api_auth`) then select `admin` set of rules.
`RULES` - set of rules which will be saved in auth token document.
Auth method and priv level can be matched with "catch all" term - `"_"`. If no exact match for auth method or priv level, then system try use "catch all" rules.
This search made at moment when token is created and result of this search will be saved in token document and applied to any further reqest, authenticated by this token.

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
`ENDPOINT_#` - any appropriate name of endpoint (`"devices"`, `"users"`, `"callflows"`, etc)
`ACCOUNT_ID_#` - any appropriate account ID
`ARG_#` - arguments for endpoint separated by `/`
`VERB_#` - any appropriate HTTP method (`"GET"`, `"PUT"`, etc)


## Match order

### Endpoint match
At this step module compare resource from URI with resource names in token restrictions.
If URI is `/v1/accounts/1234...890/users/abc/xyz/` then endpoint will be `users`, and `abc`, `xyz` is arguments of this endpoint.
Rules applied to the last endpoint in URI.
You can use "catch all" (`"_"`) endpoint name. First try found exact endpoint name, then try search for "catch all" name.

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
If not found match for endpoint- this request is halted and returned 403 error.
Each endpoint contain list of objects with rules. Appropriate object select by `"allowed_account"` parameter.

### Account match
After system found endpoint it try match account ID for this request.
```JSON
{
  "devices": [
    {
      "allowed_accounts": [
        "90b771808407e7079bfc206d5e3c1a8a",
        "83a5cb95ac8ce8bc79d6f9b148004cf4",
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
 List of account IDs set in parameter `"allowed_accounts"`. You can write axact IDs or one of special macroses.
 `"{AUTH_ACCOUNT_ID}"` - match account which created auth token.
 `"{DESCENDANT_ACCOUNT_ID}"` - match any descendats accounts.
 `"_"` - match any account.
 **If object dont have parameter `"allowed_accounts"` then it match any account (equal to `"_"`).**

 ### Endpoint arguments match
 Endpoint argumnets matched with parameter `"rules"`.
 ```JSON
{
  "devices": [
    {
      "allowed_accounts": [
        "90b771808407e7079bfc206d5e3c1a8a"
      ],
      "rules": {
        "/": [ ... ],
        "1f6d4b82b4baafc74ba83186ee04b3d5": [ ... ],
        "*": [ ... ]
      }
    }
  ]
}
```
The search is performed in the order in which they appear in the rules for first match. No more search after that.

`/` - match empty argument list (catch `/v1/accounts/90b771808407e7079bfc206d5e3c1a8a/devices`).

`*` - match any non empty argument (catch `.../devices/f61dd570039038eb97d7c656fe57ed9c`, but not `.../devices/f61dd570039038eb97d7c656fe57ed9c/sync`).

`#` - match any argument list, empty to (catch `.../devices`, `.../devices/f61dd570039038eb97d7c656fe57ed9c`, `.../devices/f61dd570039038eb97d7c656fe57ed9c/sync`, etc). Single `#` work as "catch all" rule.

`1f6d4b82b4baafc74ba83186ee04b3d5` - exact match (catch only `.../devices/1f6d4b82b4baafc74ba83186ee04b3d5`)

If you have more than one argumnets, write them in one string, seprate with `/`.You can mix the with special characters, from above.

`*/*/*` - match eactly three arguments (catch `.../devices/1f6d4b82b4baafc74ba83186ee04b3d5/quickcall/+1234567890`).

`f61dd570039038eb97d7c656fe57ed9c/#` - match `.../devices/1f6d4b82b4baafc74ba83186ee04b3d5`, `.../devices/1f6d4b82b4baafc74ba83186ee04b3d5/sync`, `.../devices/1f6d4b82b4baafc74ba83186ee04b3d5/quickcall/+1234567890`, etc.

### HTTP method match
After matching endpoint arguments, system try match HTTP method, used in this request.
 ```JSON
{
  "devices": [
    {
      "allowed_accounts": [
        "90b771808407e7079bfc206d5e3c1a8a"
      ],
      "rules": {
        "/": [
          "GET",
          "PUT"
        ],
        "1f6d4b82b4baafc74ba83186ee04b3d5": [
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
List can contain any valid HTTP method ("GET", "PUT", "POST", "PATCH", "DELETE") or "allow any" method - `"_"`.

## Manage accounts templates
### Get current account template
`GET /v1/accounts/{ACCOUNT_ID}/token_restrictions` return JSON like this:
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
If account dont have token restrictions template - return 404 error.

### Create/update account template
`POST /v1/accounts/{ACCOUNT_ID}/token_restrictions`
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
`DELETE /v1/accounts/{ACCOUNT_ID}/token_restrictions`
