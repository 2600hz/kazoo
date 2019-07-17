# CDR Task

Accounts with large volumes of CDRs find the API insufficient for downloading a month's worth of CDRs for billing purposes. This task aims to allow folks to have their CDR CSV created in the background and retrievable once finished as a task versus having to hope their dataset can be processed before the API timeout occurs.

## Available Tasks

### Dump

Dumps the current month's CDRs to a CSV file.

#### Schema

Configuration data specifically for the billing.dump task



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`from_s` | Timestamp, in Gregorian seconds, for when to start the dump | `integer()` |   | `false` |
`is_reseller` | Is the CDR CSV being generated for a reseller | `boolean()` |   | `false` |
`store_csv` | Whether to store the generated CSV using the account's storage plan | `boolean()` | `false` | `false` |
`to_s` | Timestamp, in Gregorian seconds, for when to stop the dump | `integer()` |   | `false` |
