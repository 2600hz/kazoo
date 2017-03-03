### Crossbar Auth Modules Configuration

Crossbar has multiple ways to authenticate a user or an API request.

Authentication module has their own account's version configuration to control some aspect of authentication process like enabling/disabling the module or shoyuld that modules utilized multi factor authentication.

This configuration can be set per an account which in this case if Crossbar couldn't find configuration inside the Account's database, it starts walking account's hierarchy up (going to account's parents) to find the the first account that has the configuration. If it reaches to the first reseller and still the configuration is missing then it goes to `system_configs` instead.

Configuration document inside an Account has a fixed `id` of `kazoo_auth_configs`. The module's configuration can be set inside `auth_modules` property in that document.

`auth_modules` is a JSON object which keys are the name of the exact name of Crossbar auth modules and the values are the configuration for each of the modules.

##### Sample auth configs document

This is sample configuration for `cb_user_auth` module which enables that module and set it to all authentication to be done via a multi factor service defined in `{CONFIG_ID}` document inside the same account that has the Crossbar Auth config document.

```json
{
  "_id": "kazoo_auth_configs",
  "auth_modules": {
    "cb_user_auth": {
      "enabled": true,
      "log_failed_login_attempts": true,
      "multi_factor": {
        "enabled": true,
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
`multi_factor.enabled` | should this module use multi factor for authenticating | `boolean` | `false` | `false`
`multi_factor.include_subaccounts` | if this settings comes from an account's parent, should multi factor settings been applied to the account | `boolean` | `false` | `false`
`multi_factor.configuration_id` | the `id` of the document that contains multi factor provider configs | `string` |  | `false`

#### Control Multi Factor Authentication of an Crossbar auth module

If you want to use multi factor authentication for a module, sets the `multi_factor.enabled` to `true` for that module. This is a good option to protect modules like `cb_user_auth`.

If authentication configuration comes a parent account and you don't want to use multi factor for child's account, sets `multi_factor.include_subaccounts` to `false`.

> **Note:** You can specify the `id` of multi factor provider settings. If you miss this value, system's default MFA provider will be used!

##### Multi Factor Authentication (MFA) flow summary

The MFA process in Kazoo is straight forward. You configured the Kazoo integrated MFA service provider, and enabling the multi factor for an authentication endpoint. user authentication as usual by supplying their own Kazoo username and password. After first factor authentication (username and password) is validated to true, second-factor provider information would be returned to client by a HTTP `401 unauthorized`. User performs the second-factor authentication with the provider and sends the provider response to Kazoo, if the response was valid user will be authenicated successful, otherwise a HTTP `401 unauthorized` will be returned.

For more information about MFA see [Kazoo Auth multi factor](../../../core/kazoo_auth/doc/multi_factor.md) and for configuring multi factor provider see [Crossbar multi factor](./multi_factor.md).
