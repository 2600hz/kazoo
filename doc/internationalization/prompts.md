# System Prompts

Kazoo provides many prompts, such as during voicemail, to instruct callers on things to do. The default prompts that ship with Kazoo are provided in English, but as is often the case, supporting callers who don't speak English requires alternative language prompts.

Similar to i18n efforts on the front end, Kazoo is now configurable to provide these system prompts in alternative languages. The language is selected from one of several places:

* The account processing the call
* Manually set during a callflow
* The default system language

Feature enhancements to support multi-lingual media prompts have been proudly sponsored by [CloudPBX Inc.](http://cloudpbx.ca)

L'amélioration des différents messages vocaux multilingues a été rendu possible par [CloudPBX Inc.](http://cloudpbx.ca), qui ont fièrement commandité le développement nécessaire.

## Prompt Installation

The first step, on a new installation or existing installations prior to v3.14, is to import the existing system prompts that come with Kazoo. They are the default English prompts and can be imported via SUP:

    sup kazoo_media_maintenance import_prompts /path/to/kazoo/system_media/en-us/

This will take a while to run and be a bit taxing on the BigCouch nodes, so it is advisable to run this during minimal traffic loads.

The default language associated with these prompts will be "en-us".

## Additional Prompts

As new prompts enter the system, you can selectively add them (to minimize load on the system):

    sup kazoo_media_maintenance import_prompt /path/to/kazoo/system_media/en-us/prompt_id.mp3

## Alternative Languages

For those that need translated versions of the prompts, there are a few steps required.

Create the media file and name it after the prompt-id it should represent. For instance, the prompt `menu-transferring_call` is represented in English under `$KAZOO/system_media/en-us/menu-transferring_call.wav`. You can create another version, perhaps in French, and import it thusly:

    sup kazoo_media_maintenance import_prompt /path/to/french/media/menu-transferring_call.wav fr-fr

This will add the French version of the prompt to the system_media database. If you have a whole directory of French (or other languages) prompts, use the `import_prompts` with an additional language argument:

    sup kazoo_media_maintenance import_prompts /path/to/french/media/ fr-fr
    sup kazoo_media_maintenance import_prompts /path/to/spanish/media/ es

Now, when the `menu-transferring_call` prompt is played, if the call's or account's language is set to `fr-fr`, that version of the prompt will be played for the caller.

## Cluster Default Language

To set your cluster's default language to something other than "en-us":

    sup kapps_config set_default media default_language ab-cd

## Per-Account Prompts

It is possible to create per-account prompts to be used in place of the system prompts. This is done via the Crossbar /media API.

To do so, use the standard `PUT /media` but include `prompt_id` in the data payload with the name of the prompt.

```bash
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://server.com:8000/v2/accounts/{ACCOUNT_ID}/media \
    -d '{"data":{"streamable":true,"name":"File","description":"Enter Pin prompt","prompt_id":"vm-enter_pin", "language":"x-pig-latin"}}'
```

### Configure system to allow account overrides

Make sure the system is allowing accounts to override prompts

```bash
sup kapps_config set_default "media" "support_account_overrides" true
```

### Set the account's language

Currently, a SUP command is required to set the account's language: `sup kapps_account_config set {ACCOUNT_ID} media default_language fr-ca`

You can also set it on the account doc with something like:

```bash
curl -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"langauge":"es-es"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

You can test what language will be selected for an account (barring a callflow language action changing it) thusly: `sup kz_media_util prompt_language {ACCOUNT_ID}`

### System Prompts via Crossbar

If you are a superduper admin, you can also manipulate the system_media prompts via Crossbar. Simply remove the `/accounts/{ACCOUNT_ID}` from the URL to operate against the system's prompts.

## Languages supported

While we encourage you to use proper RFC [language tags](http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.10) for identifying prompts, there is no system limitation. If the language to be used is "en-US", the system will first try to find "en-US" then fall back to "en" prompts. If the language is "fr-FR", the system will try "fr-fr" and then "fr". However, nothing stops you from doing dual-language prompts (where both languages are in one media file) and using "fr-fr_en-us". In this case, "fr-fr_en-us" will be tried, then "fr-fr", and finally "fr".

## Existing Prompts

If you've imported media from the `$KAZOO/system_media` folder, you should have a listing of all the prompts the system uses. Access this via Crossbar:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" 'http://server.com:8000/v2/media/prompts'

You can use the keys as `prompt_id` attributes when creating alternative translations.

To see what translations exist in the system for a given prompt, the listing can be obtained from Crossbar as well:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" 'http://server.com:8000/v2/media/prompts/vm-enter_pass'

## Setting the Language Per-Callflow

Add the `language` callflow action before a branch to set what language will be used when playing prompts:

    {"module":"language"
     ,"data":{"language":"en-us"}
     ,"children":{
       "_":{YOUR_FLOW_HERE}
     }
    }

This will override the account and system languages. If no language is specified, the account's language will be used; if that is missing, the system default language will be used.
