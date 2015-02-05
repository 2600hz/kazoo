/*
Section: Crossbar
Title: Search
Language: en-US
*/

The Search API allows queries on databases.

## Crossbar

### _GET_ - Fetch search results

    t = document type
    q = view name
      * all views must be defined in _design/search
      * the view used is q=name => search_by_[name]
    v = value to search

    database - the search api will search in the accountdb if the path with accountid is provided
               case accountid is not provided, will search te accounts database using authenticated account


    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" 'http://localhost:8000/v2/search?t=account&q=name&v=nat'
    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" 'http://localhost:8000/v2/accounts/xxxxxxx/search?t=user&q=name&v=j'

    {
        "page_size":1,
        "start_key":["5ba01ad7ad1611d436b1860d8c552897","account", "nat"],
        "data": [
            {
                "id": "4c004a584ca5fda8084a5bcb24430ab9",
                "name": "Natoma Office",
                "realm": "eb0dcc.sip.90e9.com"
            }
        ],
        "revision": "04e880720e84b0536389bcc4de7e69e8",
        "request_id":"53975b8ebbc595680b0da753f55c3c19",
        "status":"success"
    }


