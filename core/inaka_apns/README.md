
Apns4erl v2 [![Build Status](https://travis-ci.org/inaka/apns4erl.svg?branch=master)](https://travis-ci.org/inaka/apns4erl)
========

<img src="https://media.giphy.com/media/uZQP0PR0BmkGA/giphy.gif" align="right" style="float:right" height="400" />

This lib is intended to allow you to write an APNs provider for [Apple Push Notificaion services (APNs)](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/APNSOverview.html) over HTTP2 in Erlang.

Copyright (c) 2017 Erlang Solutions Ltd. <support@inaka.net>, released under the Apache 2 license

You can find the v1 [here](https://github.com/inaka/apns4erl/releases/tag/1.0.6-final)?

__Note:__ `Apns4erl v2` is still under development. Currently it supports push notifications with certificate and authentication token.

Contact Us
==========
If you find any **bugs** or have a **problem** while using Apns4erl, please [open an issue](https://github.com/inaka/apns4erl/issues/new) in this repo (or a pull request :)).

## Requirements
- You must have installed an updated Openssl version or, at least, be sure it supports TLS 1.2+. New APNs server only supports connections over TLS 1.2+.
- Erlang R19+

## Important Links

- [Pool of connections Example](examples/apns_pool/README.md)

## How to use it?

First we have to fill our `config` data. There are two ways for do this, one is filling a `config` file. This is an example you can find at `test/test.config`:

```
{
  apns,
  [ {apple_host,       "api.development.push.apple.com"}
  , {apple_port,       443}
  , {certfile,         "priv/apns-dev-cert.pem"}
  , {keyfile,          "priv/apns-dev-key-noenc.pem"}
  , {token_keyfile,    "priv/APNsAuthKey_KEYID12345.p8"}
  , {timeout,          10000}

  %% APNs Headers

  , {apns_id,          undefined}
  , {apns_expiration,  0}
  , {apns_priority,    10}
  , {apns_topic,       "com.example.myapp"}
  , {apns_collapse_id, undefined}

  %% Feedback
  , {feedback_host,    "feedback.push.apple.com"}
  , {feedback_port,    2195}
  ]
  ]
}
```

The other way is send all that info as a parameter to `apns:connect/1` function encapsulated in a `apns_connection:connection()` structure:

```erlang
#{ name       := name()
 , apple_host := host()
 , apple_port := inet:port_number()
 , certfile   => path()
 , keyfile    => path()
 , timeout    => integer()
 , type       := type()
 }.
```

APNs allows two connection types, one is using `Provider Certificates`. The first certificate option is to supply cert paths in `certfile` and `keyfile`. Alternatively, you can supply a cert binary in `certdata` and a `keydata()`-type tuple (see: https://github.com/inaka/apns4erl/blob/master/src/apns_connection.erl#L64) in `keydata`. Certs are the `Provider Certificates` and the keys are the `Private Key` both provided by Apple. We need them in `.pem` format, here is an example of how to convert them, check the [certificates](https://blog.serverdensity.com/how-to-build-an-apple-push-notification-provider-server-tutorial/) section.

The other way to connect against APNs is using `Provider Authentication Tokens`, for this choice you must fill the field `token_keyfile`. This is a path to the Authentication Key provided by Apple. This is in `.p8` format and it doesn't need conversion.

This `key` will be needed in order to generate a token which will be used every time we try to push a notification. In connection's time it is not needed.

## Run

`apns4erl` can be included as a dependency and started from `yourapp.app.src`. You also can run it on the shell for testing.

```
> rebar3 compile
> erl -pa _build/default/lib/*/ebin -config test/test.config
```
Don't forget your config file if you want to use `apns:connect/2`.
```erlang
1> apns:start().
ok
```

## Create connections

After running `apns4erl` app we can start creating connections. As we mentioned before there are two types of connections. Both are created using the functions `apns:connect/1` and `apns:connect/2`.

- `apns:connect/1`: This function accepts as a parameter an `apns_connection:connection()` structure.
  ```erlang
  #{ name       := name()
   , apple_host := host()
   , apple_port := inet:port_number()
   , certdata   => binary()
   , certfile   => path()
   , keydata    => keydata()
   , keyfile    => path()
   , timeout    => integer()
   , type       := type()
   }.
  ```
  where the `type` field indicates if is `certdata`, `cert`, or `token`.

- `apns:connect/2`: The first argument is the type and the second one is the connection's name. In order to use it successfully we have to fill the `config` file before, as explained in `how to use it?` section.

Example:

```erlang
1> apns:connect(cert, my_first_connection).
{ok,<0.87.0>}
2> apns:connect(#{name => another_cert, apple_host => "api.push.apple.com", apple_port => 443,
certfile => "priv/cert.pem", keyfile => "priv/key.pem", type => cert}).
3> apns:connect(token, my_second_connection).
{ok,<0.95.0>}
```
Note `cert` and `token` define the type we want.

`apns:connect/2` returns the connection `pid`.

## Create Connections without name

In some scenarios we don't want to assign names to the connections instead we want working just with the `pid` (working with a pool of connections for example). If that is the case we use the same `apns:connect/1` and `apns:connect/2` functions but instead of a connection name we put `undefined`:

```erlang
1> apns:connect(cert, undefined).
{ok,<0.127.0>}
2> apns:connect(#{name => undefined, apple_host => "api.push.apple.com", apple_port => 443,
certfile => "priv/cert2.pem", keyfile => "priv/key2-noenc.pem", type => cert}).
{ok,<0.130.0>}
3> apns:connect(token, my_second_connection).
{ok,<0.132.0>}
```

## Push Notifications over `Provider Certificate` connections

In order to send Notifications over `Provider Certificate` connection we will use `apns:push_notification/3,4`.

We will need the connection, a notification, the device ID and some http2 headers. The connection is the `atom` we used when we executed `apns:connect/2` for setting a name or its `pid`, the device ID is provided by Apple, the notification is a `map` with the data we want to send, that map will be encoded to json later and the http2 headers can be explicitly sent as a parameter using `apns:push_notification/4` or can be defined at the `config` file, in that case we would use `apns:push_notification/3`.

This is the `headers` format:

```erlang
-type headers()   :: #{ apns_id          => binary()
                      , apns_expiration  => binary()
                      , apns_priority    => binary()
                      , apns_topic       => binary()
                      , apns_collapse_id => binary()
                      , apns_auth_token  => binary()
                      }.
```

All of them are defined by Apple  [here](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html)

Lets send a Notification.

```erlang
1> {ok, Pid} = apns:connect(cert, my_first_connection).
{ok,<0.85.0>}
2> DeviceId = <<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>.
<<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>
3> Notification = #{aps => #{alert => <<"you have a message">>}}.
#{aps => #{alert => <<"you have a message">>}}
4> apns:push_notification(my_first_connection, DeviceId, Notification).
{200,
 [{<<"apns-id">>,<<"EFDE0D9D-F60C-30F4-3FF1-86F3B90BE434">>}],
 no_body}
5> apns:push_notification(Pid, DeviceId, Notification).
{200,
 [{<<"apns-id">>,<<"EFDE0D9D-F60C-30F4-3FF1-86F3B90BE654">>}],
 no_body}
```

The result is the response itself, its format is:

```erlang
-type response()  :: { integer()          % HTTP2 Code
                     , [term()]           % Response Headers
                     , [term()] | no_body % Response Body
                     } | timeout.
```

And that's all.

## Push Notifications over `Provider Authentication Tokens` connections

This is the other way APNs allows us to send notifications. In this case we don't need a certificate but we will need a `p8` file with the private key we will use to sign the token. Lets assume we've got the file  `APNsAuthKey_KEYID12345.p8` from Apple. We then have to fill the `config` file key `token_keyfile` with the path to that file.

We will need a `kid` value, this is the key identifier. In our case is the last 10 chars of the file name (`KEYID123456`). We will need also the `iss` value, this is the Team Id, that can be checked on your Apple's Developer account, in our case it will be `THEATEAM`. And that's it.

You can find more info [here](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html)

In order to push a notification we will use `apns:push_notification_token/4,5`. We will need the same attributes we used sending a notification over `Provider Certificate` connections plus a signed `token`. This token has a 1 hour life, so that means we can generate one token and use it many times till it expires. Lets try.

Create the token:

```erlang
6> TeamId = <<"THEATEAM">>.
<<"THEATEAM">>
7> KeyID = <<"KEYID123456">>.
<<"KEYID123456">>
8> Token = apns:generate_token(TeamId, KeyID).
<<"eyJhbGciOiJFUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6IktFWUlEMTIzNDU2In0.eyJpc3MiOiJUSEVBVEVBTSIsImlhdCI6MTQ4NjE0OTMzNH0.MEQC"...>>
```

Now push the notification:

```erlang
12> DeviceId = <<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>.
<<"a0dc63fb059cb9c13b03e5c974af3dd33d67fed4147da8c5ada0626439e18935">>
13> Notification = #{aps => #{alert => <<"you have a message">>}}.
#{aps => #{alert => <<"you have a message">>}}
14> apns:push_notification_token(my_second_connection, Token, DeviceId, Notification).
{200,
 [{<<"apns-id">>,<<"EBC03BF9-A784-FDED-34F7-5A8D859DA977">>}],
 no_body}
```

We can use this token for an entire hour, after that we will receive something like this:

```erlang
16> apns:push_notification_token(my_second_connection, Token, DeviceId, Notification).
{403,
 [{<<"apns-id">>,<<"03FF9497-8A6B-FFD6-B32B-160ACEDE35F0">>}],
 [{<<"reason">>,<<"ExpiredProviderToken">>}]}
```

## Pushing notifications

*NOTE* in order to push notifications, in both ways, we _must_ call `apns:push_notification/3,4` and `apns:push_notification_token/4,5` from the same
process which created the connection. If we try to do it from a different one we will get an error `{error, not_connection_owner}`.

## Reconnection

If something unexpected happens and the `chatterbox` connection with APNs crashes `apns4erl` will send a message `{reconnecting, ServerPid}` to the client process, that means `apns4erl` lost the connection and it is trying to reconnect. Once the connection has been recover a `{connection_up, ServerPid}` message will be send.

We implemented an *Exponential Backoff* strategy. We can set the *ceiling* time adding the `backoff_ceiling` variable on the `config` file. By default it is set to 10 (seconds).

## Timeout

When we call `apns:push_notification/3,4` or `apns:push_notification_token/4,5` we could get a `timeout` that could be caused if the network went down. Here is the `timeout` format:

```erlang
{timeout, stream_id()}
```
where that `stream_id()` is an identifier for the notification.


Getting a `timeout` doesn't mean your notification to APNs is lost. If `apns4erl` connects to network again it will try to send your notification, in that case `apns4erl` will send back a message to the client with the format:

```erlang
{apns_response, ServerPid, StreamID, Response}
```
where that StreamId should match with the `stream_id` we got on the `timeout` tuple.

You should check your client inbox after a timeout but it is not guaranteed your message was send successfully.

## Close connections

Apple recommends us to keep our connections open and avoid opening and closing very often. You can check the [Best Practices for Managing Connections](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/CommunicatingwithAPNs.html) section.

But when closing a connection makes sense `apns4erl` gives us the function `apns:close_connection/1` where the parameter is the connection's name or the connection's `pid`. After using it the name will be available for new connections again (if it was different than `undefined`).

## Feedback

`apns4erl` also allows us to get feedback from APNs service. It does it thru the [binary API](https://developer.apple.com/library/content/documentation/NetworkingInternet/Conceptual/RemoteNotificationsPG/BinaryProviderAPI.html).

In order to get feedback we would need a `Provider Certificate`. `apns4erl` provides us two functions, `apns:get_feedback/0` and `apns:get_feedback/1` which require some Feedback's information like url, port, timeout...  We can set that info in our `config` file and use `apns:get_feedback/0`. We can also send all that configuration as a parameter to `apns:get_feedback/1` where the config structure must looks like this:
```erlang
#{ host     := string()
 , port     := pos_integer()
 , certfile := string()
 , keyfile  => string()
 , timeout  := pos_integer()
 }.
```
The response for both functions will be a list of `feedback()`

```erlang
-type feedback() :: {calendar:datetime(), string()}.
```
Where the first element in the tuple is the date when the device uninstalled the app and the second element is the Device Id.
