## Nomorobo

### About Nomorobo

Connect to [Nomorobo](https://www.nomorobo.com/) and get a spam score on the caller. Branch based on likelihood of the call being spam.

#### Schema

Validator for the NoMoRobo callflow action



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`password` | Password to the NoMoRobo service | `string(1..1000)` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`username` | Username to the NoMoRobo service | `string(1..1000)` |   | `true` |  



