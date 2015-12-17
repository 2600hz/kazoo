gcm-erlang
=======

[![Build Status](https://api.travis-ci.org/pdincau/gcm-erlang.png)](https://travis-ci.org/pdincau/gcm-erlang)

This software provides an Erlang client for [`Google Cloud Messaging`](http://developer.android.com/google/gcm/index.html "Google Cloud Messaging for Android").


### What you can do with gcm-erlang:

Using `gcm-erlang` you can:

1. start several `gen_servers` representing different `GCM applications` defined by different `GCM API keys`
2. send notification messages to Android mobile devices registered to your specific application and registered to `GCM` using a specific `registration id`

So far `gcm-erlang` does only provide support for JSON messages since GCM does not allow to send multicast messages using plain text.

### How to compile:

The first thing you have to do is to compile all the Erlang files using `rebar`.

    $ ./rebar get-deps compile

### How to use with rebar:

You can use gcm_app as a dependency in your rebar.config:

    {deps , [
        {gcm, ".*", {git, "https://github.com/pdincau/gcm-erlang.git", {tag, "1.0.0"}}}
    ]}.

###How to run tests:

    ./rebar compile && ./rebar skip_deps=true eunit && ./run-dialyzer.sh

### How to run the application gcm-erlang:

Once all the Erlang files are compiled you can start the application `gcm-erlang`. This application depends on other applications  so it is mandatory to  start them as well.

    $ erl -pa deps/*/ebin -pa ebin
    1> application:start(inets).
    ok
    2> application:start(jsx).
    ok
    3> ssl:start().
    ok
    4> application:start(gcm).
    ok

### How to start/stop different gen_servers under application gcm-erlang (one for each GCM application):

While `gcm-erlang` is running you can start several supervised gen_servers, one for each GCM application. Every gen_server is defined by an atom used internally for registration and by a `GCM API key`.

    3> gcm:start(foo, "myapikey").
    {ok,<0.60.0>}
    4> gcm:start(bar, "myotherapikey").
    {ok,<0.65.0>}
    5> gcm:start(baz, "mylastapikey").
    {ok,<0.79.0>}

You can stop a `gen_server` representing a GCM Application using:

    6> gcm:stop(foo).

### How to send a GCM message using from a specific GCM application:

At any time you can send a GCM message to one or more mobile devices by calling:

    7> gcm:push(RegisteredName, RegIds, Message).

or by calling:

    7> gcm:sync_push(RegisteredName, RegIds, Message).

Where `RegistereName` is the atom used during registration, `RegIds` is a list (max 1000 elements) of Registration Ids specified as Erlang binaries (e.g., `<<"APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx...">>`) and `Message` is an Erlang term representing the data you want to send to the device.

The JSON message is built using `jsx` in the module `gcm.erl` and in the end will have the following form:

    {
      "registration_ids" : ["APA91bHun4MxP5egoKMwt2KZFBaFUH-1RYqx..."],
      "data" : {
        "message" : "a message"
      },
      "time_to_live" : 3600,
      "collapse_key" : "your_update"
    }

You can send this message using:

    8> gcm:push(RegisteredName, RegIds, [{<<"data">>, [
    8>     {<<"message">>, <<"a message">>}
    8> ]}, {<<"time_to_live">>,3600}, {<<"collapse_key">>,<<"your_update">>}]).

or simply:

    8> gcm:push(RegisteredName, RegIds, [{<<"data">>, [
    8>     {<<"message">>, <<"a message">>}
    8> ]}]).

`gcm-erlang` will push the message for you to `Google Cloud Messaging` servers and will parse the JSON provided as result.

In order to understand errors see: [Interpreting an error response](http://developer.android.com/google/gcm/gcm.html#response).

### Note:

Some of the concepts I used for building this Erlang application are based on this [`blog post`](http://tiliman.wordpress.com/2013/01/02/google-cloud-messaging-with-erlang/) and on this [`Erlang application for APN`](https://github.com/extend/ex_apns).
