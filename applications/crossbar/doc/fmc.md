/*
Section: Crossbar
Title: FMC application
Language: en-US
*/

The FMC is an application which provides ability to call on internal short numbers from cell phones and also transform an incoming external call from mobile phone as if it was an internal call from mobile device of existing Kazoo account.

## Configuration

The structure of the configuration document is next:

* `x_fmc_header`: A FMC SIP header of the incoming external call. When this header appears in an incoming message, we should use FMC app to process this call.
* `x_fmc_regexp`: A regular expression value used to extract needed part from the FMC SIP header (described by the `x_fmc_header` value). For example, if incoming header is `"X-FMC-Header":"FMC210001"` then we can extract number part by writing `"x_fmc_regexp":"^FMC(.+)$"` in the config doc.

Ex:
```
{
    "x_fmc_header": "X-FMC-Header",
    "x_fmc_regexp": "^X-FMC-(.+)$"
}
```

## FMC Devices

The structure of an FMC Number document is next:

* `x_fmc_value`: FMC value which will be used to distinct different call sources while the A-number can be the same.
* `device_id`: An device ID which will be used as new caller of the original call.

Ex:
```
{
    "device_id": "3edfc45563963dc267161ec868a8638e",
    "x_fmc_value": "210001"
}
```

## Using Crossbar to manage FMC app

### FMC Configuration URI

`/v2/system_configs/fmc`

#### GET - Get current configuration:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/system_configs/fmc

#### PUT - Set new configuration:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/system_configs -d '{"data": {"id": "fmc","x_fmc_header": "X-FMC-Header", "x_fmc_regexp": "^X-FMC-(.+)$"}}'

#### POST - Update existing configuration:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/system_configs/fmc -d '{"data": {"x_fmc_header": "X-FMC-Header", "x_fmc_regexp": "^X-FMC-(.+)$"}}'

### FMC Devices URI

`/v2/accounts/{ACCOUNT_ID}/fmc_devices`

#### GET - Get all existing FMC Devices:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/fmc_devices

#### GET - Get existing FMC Device by ID:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/fmc_devices/{FMC_DEVICE_ID}

#### PUT - Create new FMC Device:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/fmc_devices -d '{"data": {"device_id": "3edfc45563963dc267161ec868a8638e", "x_fmc_value": "1"}}'

#### POST - Update existing FMC Device:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/fmc_devices/{FMC_DEVICE_ID} -d '{"data": {"device_id": "3edfc45563963dc267161ec868a8638e", "x_fmc_value": "1"}}'

#### DELETE - Remove existing FMC Device:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/fmc_devices/{FMC_DEVICE_ID}
