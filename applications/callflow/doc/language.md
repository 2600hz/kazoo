/*
Section: Callflows
Title: Language
Language: en-US
*/

Prompts and other content take optional language settings to know which to fetch. Use this callflow action to set the language for the call.

## Callflow JSON

    "module":"language"
    ,"data":{"language":"fr"}

The `language` value must match with the various prompt mappings in either the account or system configuration.
