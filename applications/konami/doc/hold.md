/*
Section: Konami
Title: Hold
Language: en-US
*/

Some phones do not support putting the other line on hold or making it easy to set custom music to play while on hold. The Konami *hold* module permits both.

## How It Works

Alice is talking to Bob and would like to put him on hold while she performs a request Bob has made of her. She presses her *hold* metaflow number (say `*5`). Bob will be placed on hold, and Alice is free to talk without her audio being transmitted to Bob.

Once Alice is ready to talk with Bob again, she can use the [*resume*](./resume.md) metaflow to be reconnected with Bob.

## Configure the metaflow

The *hold* module should be placed under the "numbers" key in the "metaflows" object. It can take a custom media file to play to the on-hold party; otherwise the system default music is played.

    "metaflows":{
        "numbers":{
            "5":{
                "module":"hold"
                ,"data":{"moh":"media_id"}
            }
        }
        ,"patterns":{...}
        ,"binding_key":"*"
    }
