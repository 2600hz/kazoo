### Phone_numbers

#### About Phone_numbers

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`cnam` |   | `object` |   | `false`
`cnam.display_name` |   | `string(1..15)` |   | `false`
`cnam.inbound_lookup` |   | `boolean` |   | `false`
`dash_e911` | E911 information for the phone number | `object` |   | `false`
`dash_e911.activated_time` | The time stamp e911 was provisioned | `string` |   | `false`
`dash_e911.caller_name` | The name that will show to emergency services | `string` |   | `false`
`dash_e911.extended_address` | The suit/floor/apt. address where the number is in service | `string` |   | `false`
`dash_e911.latitude` | The e911 provisioning system calculated service address latitude | `string` |   | `false`
`dash_e911.legacy_data` | Legacy E911 information | `object` |   | `false`
`dash_e911.legacy_data.house_number` | The name that will show to emergency services | `string` |   | `false`
`dash_e911.legacy_data.predirectional` | The name that will show to emergency services | `string` |   | `false`
`dash_e911.legacy_data.streetname` | The name that will show to emergency services | `string` |   | `false`
`dash_e911.legacy_data.suite` | The name that will show to emergency services | `string` |   | `false`
`dash_e911.locality` | The locality (city) where the number is in service | `string` |   | `false`
`dash_e911.location_id` | The e911 provisioning system internal id for this service address | `string` |   | `false`
`dash_e911.longitude` | The e911 provisioning system calculated service address longitude | `string` |   | `false`
`dash_e911.plus_four` | The extended zip/postal code where the number is in service | `string` |   | `false`
`dash_e911.postal_code` | The zip/postal code where the number is in service | `string` |   | `false`
`dash_e911.region` | The region (state) where the number is in service | `string` |   | `false`
`dash_e911.status` | The e911 provisioning system status for this service address | `string('INVALID', 'GEOCODED', 'PROVISIONED', 'REMOVED', 'ERROR')` |   | `false`
`dash_e911.street_address` | The street address where the number is in service | `string` |   | `false`
`porting` | Porting (in) information for the phone number | `object` |   | `false`
`porting.billing_account_id` | The account id the losing carrier has on file | `string` |   | `false`
`porting.billing_extended_address` | The suit/floor/apt. address the losing carrier has on file | `string` |   | `false`
`porting.billing_locality` | The locality (city) the losing carrier has on file | `string` |   | `false`
`porting.billing_name` | The name or company name the losing carrier has on file | `string` |   | `false`
`porting.billing_postal_code` | The zip/postal code the losing carrier has on file | `string` |   | `false`
`porting.billing_region` | The region (state) the losing carrier has on file | `string` |   | `false`
`porting.billing_street_address` | The street address the losing carrier has on file | `string` |   | `false`
`porting.billing_telephone_number` | The BTN of the account the number belongs to | `string` |   | `false`
`porting.comments` | An array of comments | `array(string)` |   | `false`
`porting.comments.[]` |   | `string` |   | `false`
`porting.customer_contact` | The phone number that can be used to contact the owner of the number | `string` |   | `false`
`porting.port_id` | The id of the port request | `string` |   | `false`
`porting.requested_port_date` | The requested port date | `string` |   | `false`
`porting.service_provider` | The name of the losing carrier | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/check

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/check
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/locality

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/locality
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/prefix

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/prefix
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/collection
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/fix

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/fix
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/identify

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/identify
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/docs

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/docs
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/port

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/port
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/reserve

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/reserve
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/activate

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{PHONENUMBER}/activate
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers/{PHONENUMBER}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/classifiers/{PHONENUMBER}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/collection/activate

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/collection/activate
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/docs/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/docs/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/docs/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/docs/{ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/docs/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNT_ID}/phone_numbers/{ID}/docs/{ID}
```

