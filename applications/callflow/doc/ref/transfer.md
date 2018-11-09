## Transfer

### About Transfer

#### Schema

Validator for the transfer callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`captures.[]` |   | `string()` |   | `false` |  
`captures` | What to default to using if no capture group is present | `array(string())` | `["no_match"]` | `false` |  
`leg` | Which leg to transfer (transferee) | `string('self' | 'bleg')` |   | `false` |  
`target` | The target destination (extension or DID) | `string()` |   | `false` |  
`transfer_type` | The type of transfer to perform | `string('attended' | 'blind')` | `blind` | `false` |  



