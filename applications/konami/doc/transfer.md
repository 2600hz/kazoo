/*
Section: Konami
Title: Transfers
Language: en-US
*/

# In-Call Attended Transfer

## Overview
### Terminology
_2600hz:_ We are an open-source organization building large scale telecommunications software to disrupt the industry.

_Kazoo:_ This is our platform which provides telecommunications services.  Available to everyone on [github](https://github.com/2600hz/kazoo), 2600hz also offers a hosted Kazoo service.

_Kazoo API:_  The Kazoo platform exposes REST HTTP interfaces for configuration, call control, maintenance, and integration.

_Konami:_ A Kazoo application that listens to digits in active calls and detects pre-programed patters which are associated with actions.

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
Say Alice calls Bob from a phone that does not support transfers from the phone, but she needs to transfer Bob to Mary. What's a lady to do?

Fortunately, Alice has configured a metaflow that allows her to press `*21` plus the extension she wishes to dial. Since Mary's extension is 2600, Alice would press `*212600` to initiate the transfer. Bob would be put on hold, and Alice would be connected to Mary. Once Alice hangs up, Bob and Mary would be connected to begin their conversation.

#### Partial transfer

Alice doesn't have to speak with Mary first, of course. She could initiate the transfer, and before Mary answers her phone, Alice may hangup. As soon as Mary answers her phone, she will be connected with Bob.

#### What if Alice misdials

In the case that Alice dials an number that doesnt answer or rings but does answer, Alice will be reconnected with Bob once the attempt fails.

#### Cancel a transfer

Suppose Mary is taking too long to answer her phone or does not want to talk to Bob.  Alice can use the Konami [*resume*](./resume.md) code to be reconnected to Bob, stopping the call to Mary immediately.  For example, while Mary's phone is ringing Alice can dial `*22`.

### API Transfer

Using the current Kazoo API, a program would need to acquire an authentication token authorized to access the account in which the active call is being processed.  This work will produce a new API that could then be used with the authentication token to preform the transfer by POSTing the proper payload.

#### Initiating Transfer

The POST payload will be a JSON object that determines which number to initite the transfer to and the URL will contain the call-id of the transferor's call.

#### Partial Transfer

The user can preform a partial transfer by ending the call once the transfer has been started via the API.

#### Cancel a Transfer

Canceling a transfer via the API is not supported in the scope of this work.  However, if the resume Konami code is enabled on the account, the transfor can dial the in-call feature code to cancle the transfer and reconnect to the original caller.

### Configuration

An admin will be required to enable both the resume and transfer Konami metaflows for each account via the UI inorder to support in-call transfers.

The API transfer capability will be available to any authenticated API requests and has not specific configuration.

## Technical Specification

### Konami

#### Transfer Metaflow
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


#### Resume Metaflow

Fill me out

### API Transfer

#### Account Channels

     POST v1/accounts/{ACCOUNT_ID}/channels/{CALL_ID}
     {
       "data": {
         "action": "transfer",
         "target": "2600"
       }
     }
