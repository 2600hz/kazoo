### Crossbar Auth Modules Configuration

Crossbar has multiple ways to authenticate a user or an API request.

Some authentication modules has their own account's version configuration (multi_factor module for example). These configuration can be set for an account. If Crossbar couldn't find configuration inside the Account's database, it starts walking account's hierarchy up (going to account's parents) to find the the first account's that has the configurations. If it reached to the first reseller and still the configuration is missing then it goes to `system_configs` instead.

Configuration document inside an Account has a fixed `id`, `kazoo_auth_configs`. All modules' configs are inside `auth_modules` property.

`auth_modules` is a JSON object which in keys are the name of the exact name of Crossbar auth modules and the values are the configuration for each of the modules.

##### Sample auth configs document

```json
{
  "_id": "kazoo_auth_configs",
  "auth_modules": {
    "cb_user_auth": {
      "enabled": true,
      "log_failed_login_attempts": true,
      "multi_factor": {
        "enabled": false,
        "configuration_id": "{CONFIG_ID}"
      }
    }
  }
}
```

#### Auth Module settings details

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`enabled` | whether or not this authentication module is enabled | `boolean` | `true` | `false`
`log_failed_login_attempts` | should failed login attempts to be logged in database (useful for debuging) | `boolean` | `false` | `false`
`multi_factor` | control multi factor authentication for this module | `object` |   | `false`
`multi_factor.enabled` | should this module use multi factor for authenticating | `boolean` | `false | `false`
`multi_factor.include_subaccounts` | if this settings comes from an account's parent, should multi factor settings been applied to the account | `boolean` | `false` | `false`
`multi_factor.configuration_id` | the `id` of the document that contains multi factor provider configs | `string` |  | `false`

#### Control Multi Factor Authentication of an Crossbar auth module

Base on previous section, you can specify the `id` of multi factor provider settings. If you miss this values, system's default MFA provider will be used.

#### Multi Factor Authentication (MFA) flow summary

MFA in Crossbar works as below:

1. Intiate the authentication with your auth module/method with your user name and password (or token) as before.
2. If the auth method that you are using is configured to preforms the multi factor it goes to kazoo multi factor auth to process the request.
3. Based on auth module configuration, MFA provider settings will be fetched either from account's that have configured the auth module or from default provider `system_auth`
4. If no MFA provider configuration was found, the auth request will be succesful and a token will be generated as a normal auth request before
5. If the MFA provider is disabled based on the configuration, a HTTP `401 unauthorized` error will be returned.
6. Provider module will try to validate the request and settings and return a HTTP `401 unauthorized` if something is invalid and provide a appropiate error message.
7. If the request doesn't have a response from MFA provider (if `mfa_service_response` inside the body of the request) it will generate the required information that the client needs to preform with MFA provider. Crossbar ten will reply with a HTTP `401 unauthorized` with the playlod of:

```json
{
    "data": {
        "messages": "client needs to preform second-factor authentication",
        "mfa_request": { //an json object which contains the neccessary information for performing the MFA with provider}
    }
}
```

8. After the client performed the MFA with the provider, it will issue the same auth request this time with paylod included the response from MFA provider
9. In this second request, kazoo provider module will try to verifying the request, if the provider response that user is claiming could be verfied, the request will be authorized and MFA process was successful, so a token will be generated as before. Otherwise a HTTP `401 unauthorized` error will be returned.
