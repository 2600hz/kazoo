<div id="table-of-contents">
<h2>Table of Contents</h2>
<div id="text-table-of-contents">
<ul>
<li><a href="#sec-1">1. Bypass Media Mode</a>
<ul>
<li><a href="#sec-1-1">1.1. How is media handled?</a>
<ul>
<li><a href="#sec-1-1-1">1.1.1. Benefits</a></li>
<li><a href="#sec-1-1-2">1.1.2. Drawbacks</a></li>
</ul>
</li>
<li><a href="#sec-1-2">1.2. Bypassing Kazoo for media handling</a>
<ul>
<li><a href="#sec-1-2-1">1.2.1. Bypass for device(s):</a></li>
<li><a href="#sec-1-2-2">1.2.2. Bypass for resource(s):</a></li>
<li><a href="#sec-1-2-3">1.2.3. Example resource doc</a></li>
<li><a href="#sec-1-2-4">1.2.4. Benefits</a></li>
<li><a href="#sec-1-2-5">1.2.5. Drawbacks</a></li>
</ul>
</li>
<li><a href="#sec-1-3">1.3. Wrap-up</a></li>
</ul>
</li>
</ul>
</div>
</div>


# Bypass Media Mode<a id="sec-1" name="sec-1"></a>

## How is media handled?<a id="sec-1-1" name="sec-1-1"></a>

When we talk about media, we are talking about the audio packets going to and from an endpoint, where an endpoint could be your desk phone, fax machine, an upstream carrier, etc.

Let's assume you're calling a number not managed by the Kazoo installation.

When you make the phone call, the default scenario is:
1.  Your phone calls Kazoo. Specifically:
    1.  Your phone sends an INVITE to the configured Kamailio server
    2.  Kamailio forwards the INVITE to a FreeSWITCH server
    3.  Once FreeSWITCH authenticates your phone, it will then ask Kazoo for instructions on how to route your call
2.  Kazoo determines the call cannot be handled locally and thus instructs FreeSWITCH to send the call to the configured carrier(s).
    1.  FreeSWITCH sends an INVITE (different Call-ID) as instructed.
3.  Once the carrier (and resultant callee) answers, FreeSWITCH handles taking audio from your phone and sending it to the other end (and vice versa).

So what happened here?

FreeSWITCH is actually in the middle of the call, listening for your audio packets on one side and forwarding them on, as well as listening for audio packets from the other side and forwarding them to your phone. This has some benefits and some drawbacks:

### Benefits<a id="sec-1-1-1" name="sec-1-1-1"></a>

1.  Endpoints can support different codecs (ways of encoding and decoding audio). Sometimes phones don't support the same codecs and thus can't talk to each other. FreeSWITCH can transcode (convert) from one format to the other so both sides can still communicate.
2.  Features like voicemail, call recording, IVRs (that rely on DTMF from the phone), etc, require FreeSWITCH to be receiving the audio packets to work.
3.  Handles tricky NAT issues. Most phones are behind firewalls/NAT and won't accept traffic from arbitrary IP addresses.
4.  Can smooth over packet loss or delayed packets by maintaining a jitter buffer.

### Drawbacks<a id="sec-1-1-2" name="sec-1-1-2"></a>

1.  Additional latency on the packets as they traverse more network hops to reach their destination.
2.  Privacy concerns - media is transmitted "in the clear" to the FreeSWITCH server.
    a. There are ways around this using ZRTP. 'zrtp-passthru', and 'proxy-media' mode
3.  Taking media on taxes the CPUs of the FreeSWITCH server more, reducing the number of calls processable.

## Bypassing Kazoo for media handling<a id="sec-1-2" name="sec-1-2"></a>

In our default example above, there are two streams of data flowing, independent of each other: SIP signaling and RTP (the audio/video packets). In the default example, these streams both flow through FreeSWITCH. However, it is possible to instruct the endpoints to send their RTP streams directly to each other while keeping the SIP signaling going through FreeSWITCH (so you get paid!).

In Kazoo, this is controlled by the **callee's** configuration.

### Bypass for device(s):<a id="sec-1-2-1" name="sec-1-2-1"></a>

For calls to a device, the schema shows:

    "bypass_media": {
        "description": "Default bypass media mode",
        "enum": [
            true,
            false,
            "auto"
        ],
        "required": false,
        "type": [
            "boolean",
            "string"
        ],
        "warning": "The string type is deprecated, please use this as a boolean"
    },

So a device's JSON would include:

    {
      "media":{
        "bypass_media":true
      }
    }

### Bypass for resource(s):<a id="sec-1-2-2" name="sec-1-2-2"></a>

For upstream resources, the schema allows you two places:
-   Each gateway:
    
        "bypass_media": {
            "description": "The resource gateway bypass media mode",
            "required": false,
            "type": "boolean"
        },
-   On the resource's **media** object:
    
        "bypass_media": {
            "description": "Default bypass media mode",
            "required": false,
            "type": "boolean"
        },

### Example resource doc<a id="sec-1-2-3" name="sec-1-2-3"></a>

    {
      "gateways":[
        {
          "bypass_media":true
        }
      ],
      "media":{
         "bypass_media":true
      }
    }

### Benefits<a id="sec-1-2-4" name="sec-1-2-4"></a>

1.  Faster (though its always important to measure!) - Reducing the network hops the audio must traverse should improve the quality of the stream.
2.  Cheaper - less ingress and egress on your WAN<->LAN router(s) which typically means a smaller bandwidth bill.
3.  Scalability - Fewer packets to process means your infrastructure can take on more calls per-server.

### Drawbacks<a id="sec-1-2-5" name="sec-1-2-5"></a>

1.  Probably won't work for most phones behind NAT. If you're providing minutes to PBX systems, though, NAT rules are probably part of the equation already.
2.  Interop can be harder if the phones don't support the same codecs as the other side (FreeSWITCH can smooth over most of those incompatibilities).
3.  Privacy - the media will stay on the public Internet. Can be worked around but support can vary widely among phone manufacturers (and even between models and firmware of the same manufacturer).

## Wrap-up<a id="sec-1-3" name="sec-1-3"></a>

So there you have it. Pretty easy to modify your devices and carriers to support sending RTP traffic directly to the endpoint desired. Use with caution though. NAT in particular can really foul this up, leading to dead air and complaints.