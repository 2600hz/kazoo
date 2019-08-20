%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ci_analysis).

-export([new/0]).
-export([set_call_id/2, call_id/1]).
-export([set_originate_type/2, originate_type/1]).
-export([set_terminate_type/2, terminate_type/1]).
-export([set_failure_location/2, failure_location/1]).
-export([set_reason/2, reason/1]).
-export([to_json/1]).
-export([is_analysis/1]).

-record(ci_analysis, {call_id
                     ,originate_type
                     ,terminate_type
                     ,failure_location
                     ,reason
                     }).
-type analysis() :: #ci_analysis{}.

-export_type([analysis/0]).

-include("call_inspector.hrl").

-spec new() -> analysis().
new() -> #ci_analysis{}.

-spec set_call_id(analysis(), kz_term:ne_binary()) -> analysis().
set_call_id(Analysis, CallId) ->
    Analysis#ci_analysis{call_id=CallId}.

-spec call_id(analysis()) -> kz_term:api_binary().
call_id(#ci_analysis{call_id=CallId}) ->
    CallId.

-spec set_originate_type(analysis(), kz_term:ne_binary()) -> analysis().
set_originate_type(Analysis, <<"phone">> = Type) ->
    Analysis#ci_analysis{originate_type=Type};
set_originate_type(Analysis, <<"carrier">> = Type) ->
    Analysis#ci_analysis{originate_type=Type}.

-spec originate_type(analysis()) -> kz_term:api_binary().
originate_type(#ci_analysis{originate_type=Type}) -> Type.

-spec set_terminate_type(analysis(), kz_term:ne_binary()) -> analysis().
set_terminate_type(Analysis, <<"phone">> = Type) ->
    Analysis#ci_analysis{terminate_type=Type};
set_terminate_type(Analysis, <<"carrier">> = Type) ->
    Analysis#ci_analysis{terminate_type=Type}.

-spec terminate_type(analysis()) -> kz_term:api_binary().
terminate_type(#ci_analysis{terminate_type=Type}) ->
    Type.

-spec set_failure_location(analysis(), kz_term:ne_binary()) -> analysis().
set_failure_location(Analysis, <<"origination">> = Location) ->
    Analysis#ci_analysis{failure_location=Location};
set_failure_location(Analysis, <<"termination">> = Location) ->
    Analysis#ci_analysis{failure_location=Location}.

-spec failure_location(analysis()) -> kz_term:api_binary().
failure_location(#ci_analysis{failure_location=Location}) ->
    Location.

-spec set_reason(analysis(), kz_term:ne_binary()) -> analysis().
set_reason(Analysis, Reason) ->
    Analysis#ci_analysis{reason=Reason}.

-spec reason(analysis()) -> kz_term:api_binary().
reason(#ci_analysis{reason=Reason}) ->
    Reason.

-spec to_json(analysis()) -> kz_json:object().
to_json(#ci_analysis{}=Analysis) ->
    kz_json:from_list(
      [{<<"originate_type">>, originate_type(Analysis)}
      ,{<<"terminate_type">>, terminate_type(Analysis)}
      ,{<<"failure_location">>, failure_location(Analysis)}
      ,{<<"reason">>, reason(Analysis)}
      ]).

-spec is_analysis(any()) -> boolean().
is_analysis(#ci_analysis{}) -> 'true';
is_analysis(_) -> 'false'.
