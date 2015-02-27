%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(ci_analysis).

-export([new/0]).
-export([set_call_id/2
         ,call_id/1
        ]).
-export([set_originiate_type/2
        ,originiate_type/1
        ]).
-export([set_terminate_type/2
         ,terminate_type/1
        ]).
-export([set_failure_location/2
         ,failure_location/1
        ]).
-export([set_reason/2
         ,reason/1
        ]).
-export([to_json/1]).

-record(ci_analysis, {call_id
                     ,originiate_type
                     ,terminate_type
                     ,failure_location
                     ,reason
                     }).
-type ci_analysis() :: #ci_analysis{}.

-export_type([ci_analysis/0]).

-include("../call_inspector.hrl").

-spec new() -> ci_analysis().
new() -> #ci_analysis{}.

-spec set_call_id(ci_analysis(), ne_binary()) -> ci_analysis().
set_call_id(Analysis, CallId) ->
    Analysis#ci_analysis{call_id=CallId}.

-spec call_id(ci_analysis()) -> api_binary().
call_id(#ci_analysis{call_id=CallId}) ->
    CallId.

-spec set_originiate_type(ci_analysis(), ne_binary()) -> ci_analysis().
set_originiate_type(Analysis, <<"phone">> = Type) ->
    Analysis#ci_analysis{originiate_type=Type};
set_originiate_type(Analysis, <<"carrier">> = Type) ->
    Analysis#ci_analysis{originiate_type=Type}.

-spec originiate_type(ci_analysis()) -> api_binary().
originiate_type(#ci_analysis{originiate_type=Type}) -> Type.

-spec set_terminate_type(ci_analysis(), ne_binary()) -> ci_analysis().
set_terminate_type(Analysis, <<"phone">> = Type) ->
    Analysis#ci_analysis{terminate_type=Type};
set_terminate_type(Analysis, <<"carrier">> = Type) ->
    Analysis#ci_analysis{terminate_type=Type}.

-spec terminate_type(ci_analysis()) -> api_binary().
terminate_type(#ci_analysis{terminate_type=Type}) ->
    Type.

-spec set_failure_location(ci_analysis(), ne_binary()) -> ci_analysis().
set_failure_location(Analysis, <<"origination">> = Location) ->
    Analysis#ci_analysis{failure_location=Location};
set_failure_location(Analysis, <<"termination">> = Location) ->
    Analysis#ci_analysis{failure_location=Location}.

-spec failure_location(ci_analysis()) -> api_binary().
failure_location(#ci_analysis{failure_location=Location}) ->
    Location.

-spec set_reason(ci_analysis(), ne_binary()) -> ci_analysis().
set_reason(Analysis, Reason) ->
    Analysis#ci_analysis{reason=Reason}.

-spec reason(ci_analysis()) -> api_binary().
reason(#ci_analysis{reason=Reason}) ->
    Reason.

-spec to_json(ci_analysis()) -> wh_json:object().
to_json(Analysis) ->
    wh_json:from_list(
      props:filter_undefined(
        [{<<"originiate_type">>, originiate_type(Analysis)}
        ,{<<"terminate_type">>, terminate_type(Analysis)}
        ,{<<"failure_location">>, failure_location(Analysis)}
         ,{<<"reason">>, reason(Analysis)}
        ])
     ).
