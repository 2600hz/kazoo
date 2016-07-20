
### Lists

#### cURL examples

##### Get all lists (doesn't return entries)
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists

##### Get list properties (doesn't return entries)
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

##### Get list entries 
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries

##### Add new list (beware: no entries)
    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists -d '{"data": {"name": "list name"}}'

##### Replacing list (without entries)
    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists -d '{"data": {"name": "new list name"}}'

##### Updating list (without entries)
    curl -v -X PATCH -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists -d '{"data": {"description": "desc"}}'

##### Delete list and its entries
    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}}

##### Delete all entries from list
    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}}/entries

##### Add an entry to a list
    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}}/entries -d '{"data": {"number": "0123", "displayname" : "List Entry"}}'

##### List entry properties
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}}/entries/{ENTRY_ID}

##### Replace list entry
    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}}/entries/{ENTRY_ID} -d '{"data": {"number": "0123", "displayname" : "New List Entry"}}'

##### Update list entry
    curl -v -X PATCH -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}}/entries/{ENTRY_ID} -d '{"data": {"firstname" : "First name"}}'

##### Delete entry from the list
    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}}/entries/{ENTRY_ID}

##### List entry vcard
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}}/entries/{ENTRY_ID}/vcard

##### Add photo to List entry
    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}}/entries/{ENTRY_ID}/photo

#### v1 examples.

##### Get lists and their entries
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists

##### Create new list
    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists -d '{"data": {"name": "List name"}}'

##### Get list with LIST_ID
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

##### Add new entry to list
    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID} -d '{"data": {"pattern": "345"}}'

##### Rewrite list
    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID} -d '{"data": {"name": "New List name"}}'

##### Delete list
    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

##### Get entry {ENTRY_ID} from list {LIST_ID}
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/{ENTRY_ID}

##### Rewrite entry {ENTRY_ID} in list {LIST_ID}
    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/{ENTRY_ID} -d "{"data": {"132", "321"}}"

##### Delete entry from list
    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/{ENTRY_ID}
