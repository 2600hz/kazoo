# Kazoo Attachments

This application is meant to be used with [storage plans](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/doc/storage.md#plans).

## How to create a new attachment handler?

So you want to improve this app? That's wonderful!

Well, you just need to make sure the new attachment handler implements the `gen_attachment`
behaviour so they must have two callbacks:

- `put_attachment/6`
    - **Settings**::*gen_attachment:settings()*: Storage connection settings.
    - **DbName**::*gen_attachment:db_name()*: Database name where the attachment's parent document is stored.
    - **DocId**::*gen_attachment:doc_id()*: ID of the document to which the attachment will belong to.
    - **AName**::*gen_attachment:att_name()*: Attachment's file name.
    - **Contents**::*gen_attachment:contents()*: Attachment's content.
    - **Options**::*gen_attachment:options()*: Can be use to send *metadata* and/or attachment's *description*.

- `fetch_attachment/4`
    - **HandlerProps**::*gen_attachment:handler_props()*: Attachment information like `id` and/or `path` (location) within the storage service.
    - **DbName**::*gen_attachment:db_name()*: Same as for put_attachment
    - **DocId**::*gen_attachment:doc_id()*: Same as for put_attachment
    - **AName**::*gen_attachment:att_name()*: Same as for put_attachment


Anyway, if you found yourself in trouble when implementing it, check the existing attachment
handlers code and you should get a clue about how to do it yourself too.
