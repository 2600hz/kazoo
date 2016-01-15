
### Rates

How much will calls to various prefixes cost, per-minute, if applicable? Manage the rate deck via this API!

This API is accessible only to users with the `super_duper_admin` property set.

#### cURL examples

##### Summary of available rates

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/rates

##### Rate informaton for particular number

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/rates/number/1234567890

##### Create a rate

See the system_schemas/rates doc for all the available options

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/rates -d '{"data":{"prefix":"1", "iso_country_code":"US", "description":"US default rate", "rate_name":"US-1", "rate_cost":0.01}}'

##### Upload a ratedeck

For bulk uploading. CSV rows can be formatted in the following ways:

* `Prefix, ISO, Desc, Rate`
* `Prefix, ISO, Desc, InternalRate, Rate`
* `Prefix, ISO, Desc, Surcharge, InternalRate, Rate`
* `Prefix, ISO, Desc, InternalSurcharge, Surcharge, InternalRate, Rate`

A US-1 row might look like:

`1, "US-1", "US default rate", 0.01`

To upload a CSV file:

    curl -v -X POST -H "Content-Type: text/csv" -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/rates --data-binary @/path/to/rates.csv

##### Get a specific rate's information

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/rates/{RATE_ID}

##### Update a rate's definition

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/rates/{RATE_ID} -d '{"data":{"prefix":"1", "iso_country_code":"US", "description":"US default rate", "rate_name":"US-1", "rate_cost":0.03}}'

##### Delete a rate

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/accounts/{ACCOUNT_ID}/rates/{RATE_ID}
