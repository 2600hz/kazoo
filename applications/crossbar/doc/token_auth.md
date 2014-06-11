/*
Section: APIs
Title: Token Authentication
Language: en-US
*/

Authentication tokens are generated using one of the authentication endpoints exposed by Crossbar. See [User Authentication](./user_authentication.md) and [API Authentication](./api_authentication.md) as examples of generating authentication tokens.

Once you have an authentication token, you can access various Crossbar resource endpoints to manipulate the system or your account (provided you have the access).

Authentication tokens refresh their pvt_modified timestamp each time they are used in an API request. Once an authentication token's pvt_modified timestamp has passed a configurable timeout (usually one hour), it is automatically cleaned up by the system and no longer valid.

## API Endpoint

URL segment: `/token_auth`

## Sample cURL Requests

### Delete an authentication token

If you'd like to invalidate an authentication token programmatically (versus letting the system expire the token), you can issue a `DELETE`:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server.com:8000/v1/token_auth
