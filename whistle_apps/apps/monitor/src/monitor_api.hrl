-include_lib("whistle/include/wh_types.hrl").

%% Default Headers
%% All messages MUST include the DEFAULT_HEADERS list.
-define(DEFAULT_HEADERS, 
    [
      <<"Msg-ID">>
     ,<<"Server-ID">>
     ,<<"Event-Category">>
     ,<<"Event-Name">>
     ,<<"App-Name">>
     ,<<"App-Version">>
    ]).
-define(OPTIONAL_DEFAULT_HEADERS, 
    [
      <<"Job-ID">>
     ,<<"Task-Name">>
     ,<<"Task-Iteration">>
    ]).
-define(DEFAULT_VALUES, 
    [
      {<<"App-Name">>, <<"monitor">>}
     ,{<<"App-Version">>, <<"0.1.0">>}
     ,{<<"Event-Category">>, [<<"task">>]}
     ,{<<"Event-Name">>, [<<"ping_net_req">>, <<"ping_net_resp">>]}
    ]).
-define(DEFAULT_TYPES, 
    [
      {<<"Msg-ID">>, fun is_binary/1} 
     ,{<<"Server-ID">>, fun is_binary/1} 
     ,{<<"Event-Category">>, fun is_binary/1}
     ,{<<"Event-Name">>, fun is_binary/1}
     ,{<<"App-Name">>, fun is_binary/1}
     ,{<<"App-Version">>, fun is_binary/1}
     ,{<<"Job-ID">>, fun is_binary/1}
     ,{<<"Task-Name">>, fun is_binary/1}
     ,{<<"Task-Iteration">>, fun is_binary/1}
    ]).

%% The nameclature that is used for the API messages is
%% {TASK}_{AGENT SHORT NAME}_REQ|RESP_{DEFINITION} 

%% Monitor Ping Request
-define(PING_NET_REQ_HEADERS, 
    [
      <<"Destination">>
    ]).
-define(OPTIONAL_PING_NET_REQ_HEADERS, []).
-define(PING_NET_REQ_VALUES, 
    [
      {<<"Event-Category">>, <<"task">>}
     ,{<<"Event-Name">>, <<"ping_net_req">>}
    ]).
-define(PING_NET_REQ_TYPES, 
    [
      {<<"Destination">>, fun is_binary/1}
    ]).

%% Monitor Ping Respons
-define(PING_NET_RESP_HEADERS, 
    [
      <<"Success">>
    ]).
-define(OPTIONAL_PING_NET_RESP_HEADERS, 
    [
      <<"Target">>
     ,<<"TX">>
     ,<<"RX">>
     ,<<"Loss">>
     ,<<"Time">>
     ,<<"Min">>
     ,<<"Avg">>
     ,<<"Max">>
     ,<<"Mdev">>
     ,<<"Host">>
     ,<<"Error">>
    ]).
-define(PING_NET_RESP_VALUES, 
    [
      {<<"Event-Category">>, <<"task">>}
     ,{<<"Event-Name">>, <<"ping_net_resp">>}
     ,{<<"Success">>, [<<"true">>, <<"false">>]}
    ]).
-define(PING_NET_RESP_TYPES, 
    [
      {<<"Success">>, fun is_binary/1}
    ]).

%% Monitor Basic Call Request
-define(BASIC_CALL_REQ_HEADERS,
    [
      <<"Destination">>
    ]).
-define(OPTIONAL_BASIC_CALL_REQ_HEADERS, []).
-define(BASIC_CALL_REQ_VALUES,
    [
      {<<"Event-Category">>, <<"task">>}
     ,{<<"Event-Name">>, <<"basic_call_req">>}
    ]).
-define(BASIC_CALL_REQ_TYPES,
    [
      {<<"Destination">>, fun is_binary/1}
    ]).

%% Monitor Basic Call Response
-define(BASIC_CALL_RESP_HEADERS,
    [
      <<"Success">>
    ]).
-define(OPTIONAL_BASIC_CALL_RESP_HEADERS,
    [
      <<"Delay">>
     ,<<"Error">>
    ]).
-define(BASIC_CALL_RESP_VALUES,
    [
      {<<"Event-Category">>, <<"task">>}
     ,{<<"Event-Name">>, <<"basic_call_resp">>}
     ,{<<"Success">>, [<<"true">>, <<"false">>]}
    ]).
-define(BASIC_CALL_RESP_TYPES,
    [
      {<<"Success">>, fun is_binary/1}
    ]).
