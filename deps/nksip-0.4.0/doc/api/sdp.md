# NkSIP SDP API

Function|Description
---|---
[new/2](#new2)|Generates a simple base SDP record
[new/0](#new0)|Generates a simple base SDP record using host `"auto.nksip"`
[empty/0](#empty0)|Generates an empty SDP record, using host `"auto.nksip"`
[increment/1](#increment1)|Increments the SDP version by one
[update/2](#update2)|Updates and SDP changing all medias
[is_sdp/1](#is_sdp1)|Checks if term is an valid SDP
[is_new/2](#is_new2)|Checks if `SDP2` is newer than `SDP1`
[parse/1](#parse1)|Parses a binary SDP packet
[unparse/1](#unparse1)|Generates a binary SDP packet from an `nksip_sdp:sdp()` record.

## Example

```erlang
1> SDP = nksip_sdp:new("local", [
        {<<"audio">>, 10000, [{rtpmap, 0, "Params0"}, {rtpmap, 1, "Params1"}, sendrecv]},
        {<<"video">>, 10001, [{rtpmap, 2, "Params2"}, {rtpmap, 3, "Params3"}, sendrecv]}
   ]).
#sdp{sdp_vsn = <<"0">>,user = <<"-">>,id = 1405007035,
     vsn = 1405007035,
     address = {<<"IN">>,<<"IP4">>,<<"local">>},
     session = <<"nksip">>,info = undefined,uri = undefined,
     email = undefined,phone = undefined,
     connect = {<<"IN">>,<<"IP4">>,<<"local">>},
     bandwidth = [],
     time = [{0,0,[]}],
     zone = undefined,key = undefined,attributes = [],
     medias = [#sdp_m{media = <<"audio">>,port = 10000,
                      nports = 1,proto = <<"RTP/AVP">>,
                      fmt = [<<"0">>,<<"1">>],
                      info = undefined,connect = undefined,bandwidth = [],
                      key = undefined,...},
               #sdp_m{media = <<"video">>,port = 10001,nports = 1,
                      proto = <<"RTP/AVP">>,
                      fmt = [<<"2">>,<<"3">>],
                      info = undefined,connect = undefined,bandwidth = [],...}]}
                      
2> io:format("~s", [nksip_sdp:unparse(SDP)]).
v=0
o=- 1405007035 1405007035 IN IP4 local
s=nksip
c=IN IP4 local
t=0 0
m=audio 10000 RTP/AVP 0 1
a=rtpmap:0 Params0
a=rtpmap:1 Params1
a=sendrecv
m=video 10001 RTP/AVP 2 3
a=rtpmap:2 Params2
a=rtpmap:3 Params3
a=sendrecv
ok
```


## API

### new/2
```erlang
nksip_sdp:new(Host::string()|binary(), MediaSpecs::[nksip_sdp:media_spec()]) -> 
    nksip_sdp:sdp().
```
    
Generates a simple base SDP record. 

It will use the indicated `Host` and a `MediaSpecs` description to generate a new `nksip_sdp:sdp()` record, having several `m` sections, one for each media. 

Each media must define a `Media` (like `<<"audio">>` or `<<"video">>`), a `Port` and a list of `Attributes`. Each attributes can have the form `{rtpmap, Pos, Data}` to define a codec (like `{rtpmap, 0, <<"PCMU/8000">>}`) or a standard SDP `a` attribute (like `<<"inactive">>` or `<<"ptime:30">>`). The class will be `RTP/AVP`.

If `Host` is `"auto.nksip"`, NkSIP it will be changed to the current local address
before sending.




### new/0
```erlang
nksip_sdp:new() ->
    nksip_sdp:sdp().
```

Generates a simple base SDP record (see [new/2](#new2), using host `"auto.nksip"`, port `1080`, codec `"PCMU"`, and `inactive`.


### empty/0
```erlang
nksip_sdp:empty() ->
    nksip_sdp:sdp().
```

Generates an empty SDP record, using host `"auto.nksip"` (see [new/2](#new2)).
Equivalent to `new(<<"auto.nksip">>, [])`.


### increment/1
```erlang
nksip_sdp:increment(nksip_sdp:sdp()) ->
    nksip_sdp:sdp().
```

Increments the SDP version by one.


### update/2
```erlang
nksip_sdp:update(nksip_sdp:sdp(), inactive | recvonly | sendonly | sendrecv) ->
    nksip_sdp:sdp().
```

Updates and SDP changing all medias to `inactive`, `recvonly`, `sendonly` or `sendrecv` and incrementing the SDP version.


### is_sdp/1
```erlang
nksip_sdp:is_sdp(term()) ->
    boolean().
```

Checks if term is an valid SDP.


### is_new/2
```erlang
nksip_sdp:is_new(SDP2::undefined|nksip_sdp:sdp(), SDP1::undefined|nksip_sdp:sdp()) ->
    boolean().
```

Checks if `SDP2` is newer than `SDP1`.
If any of them are `undefined`, returns `false`.

### parse/1
```erlang
nksip_sdp:parse(binary()) -> 
    nksip_sdp:sdp() | error.
```

Parses a binary SDP packet into a `nksip_sdp:sdp()` record or `error`.

### unparse/1
```erlang
nksip_sdp:unparse(nksip_sdp:sdp()) -> 
    binary().
```

Generates a binary SDP packet from an `nksip_sdp:sdp()` record.


