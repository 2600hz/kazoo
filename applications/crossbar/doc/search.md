/*
Section: Crossbar
Title: Search
Language: en-US
*/

The Search API allows queries on databases in the form field=value.

## Crossbar

### _GET_ - Fetch search results

    c = Context where to search
       * current limitation "accounts"
    q = query in the form field=value
       * q must be uri encoded "name=xyz" => "name%3Dxyz"
       * current limitation , only handles field "name"

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" 'http://localhost:8000/v2/search?c=accounts&q=name%3Dnat'

    {
        "page_size":1,
        "start_key":["5ba01ad7ad1611d436b1860d8c552897","nat"],
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


