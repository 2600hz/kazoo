# In-Call Attended Transfer

## Overview

### Terminology
_2600hz:_ We are an open-source organization building large scale telecommunications software to disrupt the industry.

_Kazoo:_ This is our platform which provides telecommunications services.  Available to everyone on [github](https://github.com/2600hz/kazoo), 2600Hz also offers a hosted Kazoo service.

_Kazoo API:_  The Kazoo platform exposes REST HTTP interfaces for configuration, call control, maintenance, and integration.

_Konami:_ A Kazoo application that listens to digits in active calls and detects pre-programmed patters which are associated with actions.

### Purpose
The purpose is of this project is to allow callers to preform an attended transfer from their mobile device.

### Non-Goals

### Scenarios

#### App-less Transfer

As a user when I on a call with someone I would like the ability to preform an attended transfer by dialing some feature code into the native dialer.

#### App-based Transfer
As a user using an application I installed on my mobile device, I would like to be able to press a transfer button that makes an attended transfer.

### Prerequisites

## Functional Specification

Some phones do not support the ability to initiate transfers. The Konami *transfer* module will permit those devices to initiate transfers via star codes.

Additional, transfer will be able to be initiated via Kazoo API requests.

### In-Call Feature Codes

Say Alice calls Bob from a phone that does not support transfers from the phone, but she needs to transfer Bob to Carol. What's a lady to do?

Fortunately, Alice has configured a metaflow that allows her to press `*2` plus the extension she wishes to dial. Since Carol's extension is 2600, Alice would press `*22600` to initiate the transfer. Bob would be put on hold, and Alice would be connected to Carol. Once Alice hangs up, Bob and Carol would be connected to begin their conversation.

#### Partial transfer

Alice doesn't have to speak with Carol first, of course. She could initiate the transfer, and before Carol answers her phone, Alice may hangup. As soon as Carol answers her phone, she will be connected with Bob.

#### What if Alice misdials

In the case that Alice dials an number that doesn't answer or rings but does answer, Alice will be reconnected with Bob once the attempt fails.

#### Cancel a transfer

Suppose Carol is taking too long to answer her phone or does not want to talk to Bob. She can configure her *transfer* metaflow to listen for a DTMF sequence (called `takeback_dtmf`) to immediately reconnect her with Bob and cancel the call to Carol. The `takeback_dtmf` defaults to `*1` unless overridden in the data portion of the metaflow.

### API Transfer

Using the current Kazoo API, a program would need to acquire an authentication token authorized to access the account in which the active call is being processed.  This work will produce a new API that could then be used with the authentication token to preform the transfer by POSTing the proper payload.

#### Initiating Transfer

The POST payload will be a JSON object that determines which number to initiate the transfer to and the URL will contain the call-id of the transferor's call.

#### Partial Transfer

The user can preform a partial transfer by ending the call once the transfer has been started via the API.

#### Cancel a Transfer

Canceling a transfer via the API is not supported in the scope of this work.  However, if the resume Konami code is enabled on the account, the transferor can dial the in-call feature code to cancel the transfer and reconnect to the original caller.

### Configuration

An admin will be required to enable both the resume and transfer Konami metaflows for each account via the UI in order to support in-call transfers.

The API transfer capability will be available to any authenticated API requests and has not specific configuration.

## Technical Specification

### Konami

#### Transfer Metaflow

The *transfer* module requires being used with Metaflow "patterns". In the example above, Alice's metaflow would contain:

```json
    "metaflows":{
        "numbers":{...}
        ,"patterns":{
            "2(\\d+)":{
                "module":"transfer"
                ,"data":{
                    "takeback_dtmf":"*1"
                    ,"moh":"media_id"
                }
            }
        }
        ,"binding_key":"*"
    }
```

##### Configuration

* `takeback_dtmf`: the DTMF sequence the transferor can press to reattach to the transferee leg, canceling the transfer
* `moh`: a media_id from the account's media holdings, to play to the transferee instead of the default system music-on-hold
* `ringback`: the tone stream to play to the transferor when the target call leg is being setup. See the [FreeSWITCH TGML](http://wiki.freeswitch.org/wiki/TGML) for samples. Otherwise the `default_ringback` in the `system_config/ecallmgr` doc will be used (if set).

### API Transfer

#### Account Channels

This requires the cb_channels module to be started: `sup crossbar_maintenance start_module cb_channels`

```json
     POST v2/accounts/{ACCOUNT_ID}/channels/{CALL_ID}
     {
       "data": {
         "action": "transfer",
         "target": "2600",
         "takeback_dtmf": "*1",
         "moh": "media_id"
       }
     }
```

##### Configuration

* `action`: what action to perform on the call
* `target`: the extension or DID to dial for the target leg of the transfer
* `takeback_dtmf`: the DTMF sequence the transferor can press to reattach to the transferee leg, canceling the transfer
* `moh`: a media_id from the account's media holdings, to play to the transferee instead of the default system music-on-hold
