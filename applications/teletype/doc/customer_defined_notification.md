Customer defined notification can use this macros

## System
macros | description
-------|------------
hostname | the system hostname
node | the system node

## Account
macros | description
-------|------------
id | unique id string of account
language | configured account language, may be different with user language. Example `en-US`, `ru-RU`.
name | unique text description of account
parrent_id | unique id string of parrent account
parrent_name | unique text description of parrent account
parrent_realm | realm(domain) name of parrent account. Example `example.company.com`
realm | realm(domain) name of account. Example `example.company.com`
timezone | configured account timezone. Example `America/Los_Angeles`, `Europe/Moscow`

## User
macros | description
-------|------------
email | user email address. Example `bigboss@company.com`
first_name | user first name. Example `Janet`
last_name | user last name. Example `Devlin`
timezone | configured user timezone. Example `America/Los_Angeles`, `Europe/Moscow`
username | user username, used for user authentication

## Call
macros | description
-------|------------
timestamp | event timestamp in gregorian format. Example `63709408440`
to_user | username of `To` call header
to_realm | realm(domain) of `To` call header
from_user | username of `From` call header
from_realm | realm(domain) of `From` call header
call_id | `Call-ID` header of caller or callee leg in call
caller_id_number | caller number - this value may be present in `P-Asserted-Identity`, `Remote-Party-ID` or `From` call headers
caller_id_name | caller name - text description of caller, this value may be present in `P-Asserted-Identity`, `Remote-Party-ID` or `From` call headers
