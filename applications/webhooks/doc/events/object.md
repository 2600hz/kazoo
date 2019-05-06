# Object (JSON Object) change events

Triggers when objects (like JSON document objects) of certain types in Kazoo are changed.

## Info

* **Event name:** object
* **Friendly name:** Object

## Modifiers

To restrict the kind of document or the action or both, you can set the custom data to:

* **type:** A list of object types to handle:
    * `account`
    * `call_recording`
    * `callflow`
    * `device`
    * `fax`
    * `faxbox`
    * `mailbox_message`
    * `media`
    * `user`
    * `vmbox`
* **action:** A list of object actions to handle:
    * `doc_created`
    * `doc_deleted`
    * `doc_edited`

## Samples

`type` and `action` fields reflect the selected modifiers above:

```json
{
    "id": "c4c0ad092e57bc1d28e69bbd20dad932",
    "account_id": "0e10f9365fb2383d0fa65e907bfe4cb3",
    "action": "doc_created",
    "type": "user"
}
```
