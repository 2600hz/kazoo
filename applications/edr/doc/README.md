# EDR (SIPLABS LLC)

## Description

EDRs (Event Data Records) provides a summary view of all events in the
system.  The EDR is a list of attributes related to the processing and
execution of some works in the system.

Basic idea is simple - when event an occurs somewhere in the system
and if it is important enough, kz_edr:log_event function is called
with parameters that describes this event. After that, EDR going into
one of the EDR application instances. Then, this app forwards EDR-data
to all running backends that are configured to receive events of this
kind.

## Sending EDR

If you develop your own kazoo application or something like that, you
probably want to indicate some events that might be useful for those
who will use or administrate this application, giving them the
opportunity to receive an event notifications by any convenient way.
Curently supported backend types:

- file - backend for developer testing.  Options:
  - Path - path to file
- elasticsearch - send EDRs to your elasticseatch server.  Options:
  - Url - address of elasticsearch server
  - Connect-Timeout - time in ms for send EDR
  - Response-Timeout - time in ms for response. If no response, marks
    as error
  - Is-SSL - use secure sockets layer protocol
  - Max-Sessions - maximum amount of concurrent sendings.

The main function that you need is kz_edr:log_event(EventCategory,
EventName, Tags, AppName, AppVersion), where

- EventCategory: categories() - classifies an event to the some category,
- EventName : binary() - gives name to event,
- Tags : kz_json:object() - for additional data,
- AppName : in which application the event occurred,
- AppVersion : binary() - code verion

where categories() is one of the following binaries (in developing):
- `<<"application">>` - category to group events, specific for this
  application. For example
  ```
  > Data = kz_json:set_value(<<"any_key">>, <<"any_data">> , kz_json:new()),
  > kz_edr:event_log(<<"application">>, <<"callflow">>, Data, ?APP_NAME, ?APP_VERSION).
  ```
  tell us, that callflow was used. If you need event that contains
  more specific information, you can use Tags argument.

## Receiving EDR

To receive EDR: you must register or just start your backend in
application. Backend - is a process for handle\store EDRs. Use next
functions:

- edr_backend_sup:start_backend(Name:: ne_binary(), Type ::
  ne_binary(), Tags :: kz_json:object(), Options :: kz_json:object())
  - to start new backend instance, for example:

  ```
  > Tags = kz_json:set_value(<<"App-Name">>, <<"callflow">>, kz_json:new()),
  > Options = kz_json:set_value(<<"Url">>, "localhost:9200/logs", kz_json:new()),
  > edr_backend_manager:start_backend(<<"my_elastic">>, <<"elasticsearch">>, Tags, Options).
  > {ok,<0.2252.0>}
  ```
- edr_backend_sup:stop_backend(Id) - to stop backend, where Id is name
  or pid of existing backend process, that may be obtained with
  get_running_backends/0.
- edr_utils:register_backend(Name, Type, Tags, Opts, IsEnable) - to
  add backend record to autoload list. Arguments same as
  start_backend, but have additional argument IsEnable.
- edr_utils:delete_backend(Name) - to remove backend record from
  autoload list.
- edr_utils:enable_backend(Name) - to turn backend on, if it have
  state 'enabled' == 'false'.
- edr_utils:disable_backend(Name) - to turn backend off, if it have
  state 'enabled' == 'true'.
- edr_utils:registred_backends/0 - returns list of backends, presented
  in autoload list.  Example of return:
  ```
  > edr_utils:registred_backends().
  > {[{<<"my_elastic">>,
             {[{<<"Name">>,<<"my_elastic">>},
             {<<"Options">>,
             {[{<<"Url">>,
                     "http://elastictest1.siplabs.local:9200/logs/external/5"}]}},
             {<<"Tags">>,{[{<<"App-Name">>,<<"callflow">>}]}},
             {<<"Type">>,<<"elasticsearch">>},
             {<<"Enabled">>,true}]}},
     {<<"My_file_backend">>,
             {[{<<"Name">>,<<"My_file_backend">>},
             {<<"Options">>, {[{<<"Path">>,<<"test.txt">>}]}},
             {<<"Tags">>, {[{<<"App-Name">>,<<"callflow">>}]}},
             {<<"Type">>,<<"file">>},
             {<<"Enabled">>,true}]}}]}
  ```
⁃ edr_backend_sup:get_running_backends/0 - returns list of curently
  running backends, as shown below:
  ```
  > edr_utils:get_running_backends().
  > [{<<"My_file_backend">>,<0.2984.0>,[edr_file]},
     {<<"my_elastic">>,<0.2252.0>,[edr_elasticsearch]}]
  ```
