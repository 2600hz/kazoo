/*
Section: Konami
Title: Transfers
Language: en-US
*/

Some phones do not support the ability to initiate transfers. The Konami *transfer* module will permit those devices to initiate transfers via star codes.

## How It Works

Say Alice calls Bob from a phone that does not support transfers from the phone, but she needs to transfer Bob to Mary. What's a lady to do?

Fortunately, Alice has configured a metaflow that allows her to press `*21` plus the extension she wishes to dial. Since Mary's extension is 2600, Alice would press `*212600` to initiate the transfer. Bob would be put on hold, and Alice would be connected to Mary. Once Alice hungup, Bob and Mary would be connected to begin their conversation.

#### Unattended transfer

Alice doesn't have to speak with Mary first, of course. She could initiate the transfer, and before Mary answers her phone, Alice may hangup. As soon as Mary answers her phone, she will be connected with Bob.

#### What if Mary doesn't answer?

In the case that Mary doesn't answer (or rejects the call, or otherwise fails), Alice will be reconnected with Bob once the attempt to contact Mary fails.

#### Cancel a transfer

Suppose Alice initiates a transfer to a wrong extension, or decides Mary is taking too long to answer her phone and Alice doesn't want to wait for the full time. Alice can use the Konami [*resume*](./resume.md) code to be reconnected to Bob, stopping the call to Mary immediately.

## Configure the metaflow

The *transfer* module requires being used with Metaflow "patterns". In the example above, Alice's metaflow would contain:

    "metaflows":{
        "numbers":{...}
        ,"patterns":{
            "21(\\d+)":{
                "module":"transfer"
                ,"data":{}
            }
        }
        ,"binding_key":"*"
    }
