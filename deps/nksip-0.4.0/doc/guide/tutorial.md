Tutorial
========

This a simple tutorial covering the basic aspects of NkSIP.


Firt of all install last stable version of NkSIP:
```
> git clone https://github.com/kalta/nksip
> cd nksip
> git checkout v0.4.0 -q
> make
> make tutorial
```

Now you can start a simple SipApp proxy server using the included [server callback module](../../samples/nksip_tutorial/src/nksip_tutorial_sipapp_server.erl). We will listen on all interfaces, port `5060` for udp and tcp and `5061` for tls. We also activate the [nksip_registrar](../plugins/registrar.md) plugin to tell NkSIP to process registrations:
```erlang
1> nksip:start(server, nksip_tutorial_sipapp_server, [], 
        [
            {plugins, [nksip_registrar]},
            {transports, [{udp, all, 5060}, {tls, all, 5061}]}
         ]).
{ok,armejl7}
```

NkSIP returns the _internal name_ of the application, which is alwats an `atom()`, calculated as a hash over the name.

Now we can start two clients, using the included [client callback module](../../samples/nksip_tutorial/src/nksip_tutorial_sipapp_client.erl). The first is called client1 and listens on `127.0.0.1` ports `5070` for udp and tcp and `5071` for tls, and the second is called client2 and listens on all interfaces, random ports. We also configure the _From_ header to be used on each one, the second one using `sips`.

We also activate the [nksip_uac_auto_auth](../plugins/auto_auth.md) plugin to generate automatic digest authentications:

```erlang
2> nksip:start(client1, nksip_tutorial_sipapp_client, [], 
        [
            {plugins, [nksip_uac_auto_auth]},
            {from, "sip:client1@nksip"},
            {transports, [{udp, {127,0,0,1}, 5070}, {tls, {127,0,0,1}, 5071}]}
        ]).
{ok,b3finlv}
3> nksip:start(client2, nksip_tutorial_sipapp_client, [], 
        [
            {plugins, [nksip_uac_auto_auth]},
            {from, "sips:client2@nksip"},
            {transports, [udp, tls]}
        ]).
{ok,ayr8ncm}

```

From now on, you could start tracing to see all SIP messages on the console, using 

```erlang
4> nksip_trace:start().
[{client2,ok},{client1,ok},{server,ok}]
```

Let's try now to send an _OPTIONS_ from client2 to client1 and from client1 to the server:
```erlang
5> nksip_uac:options(client2, "sip:127.0.0.1:5070", []).
{ok,200,[]}
6> nksip_uac:options(client1, "sip:127.0.0.1", [{meta,[reason_phrase]}]).
{ok,407,[{reason_phrase, <<"Proxy Authentication Required">>}]}
```

Oops, the server didn't accept the request (we have used the `meta` option to 
order NkSIP to return the reason phrase). 

In the client callback module there is no authentication related callback function implemented, so every request is accepted. But server callback module is different:

```erlang
%% @doc Called to check user's password.
%%
%% If the incoming user's realm is "nksip", the password for any user is "1234". 
%% For other realms, no password is valid.
%%
sip_get_user_pass(_User, <<"nksip">>, _Req, _Call) -> 
    <<"1234">>;
sip_get_user_pass(_User, _Realm, _Req, _Call) -> 
    false.


%% @doc Called to check if a request should be authorized.
%%
%% 1) We first check to see if the request is an in-dialog request, coming from 
%%    the same ip and port of a previously authorized request.
%%
%% 2) If not, we check if we have a previous authorized REGISTER request from 
%%    the same ip and port.
%%
%% 3) Next, we check if the request has a valid authentication header with realm 
%%    "nksip". If `{{digest, <<"nksip">>}, true}' is present, the user has 
%%    provided a valid password and it is authorized. 
%%    If `{{digest, <<"nksip">>}, false}' is present, we have presented 
%%    a challenge, but the user has failed it. We send 403.
%%
%% 4) If no digest header is present, reply with a 407 response sending 
%%    a challenge to the user.
%%
sip_authorize(AuthList, _Req, _Call) ->
    case lists:member(dialog, AuthList) orelse lists:member(register, AuthList) of
        true -> 
            ok;
        false ->
            case proplists:get_value({digest, <<"nksip">>}, AuthList) of
                true -> 
                    ok;            % Password is valid
                false -> 
                    forbidden;     % User has failed authentication
                undefined -> 
                    {proxy_authenticate, <<"nksip">>}
                    
            end
    end.
```

We try again with a correct password. In the second case we are telling NkSIP to 
use `tls` transport. Note we must use `<` and `>` if including `uri` parameters like `transport`.
```erlang
7> nksip_uac:options(client1, "sip:127.0.0.1", [{pass, "1234"}]).
{ok,200,[]}
8> nksip_uac:options(client2, "<sip:127.0.0.1;transport=tls>", [{pass, "1234"}]).
{ok,200,[]}
```

Both requests receive a 407 response, but the nksip_uac_auto_auth plugin takes the included password and generates a new request with the correct headers, that are accepted at the server.

Let's register now both clients with the server. We use the option `contact` to tell NkSIP to include a valid _Contact_ header in the request, and the `mtea` option to get the _Contact_ header from the response, to be sure the server has stored the contact:

```erlang
9> nksip_uac:register(client1, "sip:127.0.0.1", 
                      [{pass, "1234"}, contact, {meta, [<<"contact">>]}]).
{ok,200,[{<<"contact">>, [<<"<sip:client1@127.0.0.1:5070>...">>]}]}
10> nksip_uac:register(client2, "sips:127.0.0.1", [{pass, "1234"}, contact]).
{ok,200,[]}
```

We can check this second registration has worked. If we send a _REGISTER_ request with no _Contact_ header, the server will include one for each stored registration. This time, lets get all the header from the response using `all_headers` as field specification:

```erlang
11> nksip_uac:register(client2, "sips:127.0.0.1", [{pass, "1234"}, {meta, [all_headers]}]).
{ok,200,[{all_headers, [{<<"call-id">>, ...}]}]}
```

Now, if we want to send the same _OPTIONS_ again, we don't need to include the authentication, because the origins of the requests are already registered:
```erlang
12> nksip_uac:options(client1, "sip:127.0.0.1", []).
{ok,200,[]}
13> nksip_uac:options(client2, "sips:127.0.0.1", []).
{ok,200,[]}
```

Now let's send an _OPTIONS_ from client1 to client2 through the proxy. As they are already registered, we can use their registered names or _address-of-record_. We use the option `route` to send the request to the proxy (you usually include this option in the call to `nksip:start/4`, to send _every_ request to the proxy automatically).

The first request is not authorized. The reason is that we are using a `sips` uri as a target, so NkSIP must use tls. But the origin port is then different from the one we registered, so we must authenticate again:

```erlang
14> nksip_uac:options(client1, "sips:client2@nksip", [{route, "<sip:127.0.0.1;lr>"}]).
{ok,407,[]}
15> nksip_uac:options(client1, "sips:client2@nksip", 
                          [{route, "<sip:127.0.0.1;lr>"}, {pass, "1234"},
                           {meta, [<<"x-nk-id">>]}]).
{ok,200,[{<<"x-nk-id">>, [<<"client2">>]}]}
```
In the second case we want to get the _X-Nk-Id header from the response (in NkSIP, all headers must be spelled lowercase).
Our callback `options/2` is called for every received options request, which includes the custom header:

```erlang
sip_options(Req, _Call) ->
    {ok, AppName} = nksip_request:app_name(Req),
    {reply, {ok, [{add, "x-nk-id", AppName}, contact, allow, accept, supported]}}.
```

Now let's try a _INVITE_ from client2 to client1 through the proxy. NkSIP will call the callback `invite/2` in client1's callback module:

```erlang
sip_invite(Req, _Call) ->
    {ok, Body} = nksip_request:body(Req),
    case nksip_sdp:is_sdp(Body) of
        true ->
            {ok, ReqId} = nksip_request:get_handle(Req),
            Fun = fun() ->
                nksip_request:reply(ringing, ReqId),
                timer:sleep(2000),
                nksip_request:reply({answer, Body}, ReqId)
            end,
            spawn(Fun),
            noreply;
        false ->
            {reply, {not_acceptable, <<"Invalid SDP">>}}
    end.
```

In the first call, since we don't include a body, client1 will reply `not_acceptable` (code `488`).
In the second, we _spawn_ a new process, reply a _provisional_ `180 Ringing`, wait two seconds and reply a `final` `200 Ok` with the same body. For 2xx _INVITE_ responses, NkSIP will allways include the `dialog` value.

We include the option `auto_2xx_ack` for NkSIP to generate the mandatory ACK automatically, instead of having to call 
to `nksip_uac:ack(DlgId, [])` manually inmeditaly after the 2xx response.

```erlang
16> nksip_uac:invite(client2, "sip:client1@nksip", [{route, "<sips:127.0.0.1;lr>"}]).
{ok,488,[]}
17> {ok,200,[{dialog, DlgId}]} = nksip_uac:invite(client2, "sip:client1@nksip", 
                                        [
                                            {route, "<sips:127.0.0.1;lr>"}, 
                                            {body, nksip_sdp:new()},
                                            auto_2xx_ack
                                        ]).
{ok,200,[{dialog, <<"...">>}]}	
```

If we hadn't included the `auto_2xx_ack`, we should have sent the mandatory ACK for 2xx responses:

```erlang
18> nksip_uac:ack(DlgId, []),
ok
```

The call is accepted and we have started a _dialog_:
```erlang
19> nksip_dialog:meta(invite_status, DlgId).
{ok, confirmed}
```

You can _print_ all dialogs in the console. We see dialogs at client1, client2 and at the server. The three dialogs are the same actually, but in different SipApps (do not use this command in production with many thousands of dialogs):
```erlang
20> nksip_dialog:get_all_data().
[...]
```

Ok, let's stop the call, the dialogs and the SipApps:
```erlang
21> nksip_uac:bye(DlgId, []).
{ok,200,[]}
22> nksip:stop_all().
ok
```

The full code for this tutorial is available [here](../../samples/nksip_tutorial/src/nksip_tutorial.erl).



















