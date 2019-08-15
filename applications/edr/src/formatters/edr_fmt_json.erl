%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2017, Conversant Ltd
%%% @doc JSON formatter module.
%%% @author Max Lay
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(edr_fmt_json).

-export([format_event/2]).

-export([to_jobj/2]).

-behaviour(edr_formatter).

-include("../edr.hrl").

-spec format_event(kz_json:object(), edr_event()) -> kz_term:ne_binary().
format_event(Opts, Event) ->
    encode(Opts, to_jobj(Opts, Event)).

-spec to_jobj(kz_json:object(), edr_event()) -> kz_json:object().
to_jobj(Opts, Event) ->
    to_jobj(Opts, Event, kz_json:is_true(<<"include_metadata">>, Opts, 'true')).

-spec to_jobj(kz_json:object(), edr_event(), boolean()) -> kz_json:object().
to_jobj(_Opts, #edr_event{body=JObj}, 'false') ->
    JObj;
to_jobj(Opts, Event, 'true') ->
    to_jobj(Opts, Event, 'true', kz_json:get_value(<<"normalize">>, Opts, 'true')).

-spec to_jobj(kz_json:object(), edr_event(), boolean(), boolean()) -> kz_json:object().
to_jobj(_Opts, Event, _IncludeMeta, 'false') ->
    Props = [{<<"Account-ID">>, Event#edr_event.account_id}
            ,{<<"Account-Tree">>, Event#edr_event.account_tree}
            ,{<<"App-Name">>, Event#edr_event.app_name}
            ,{<<"App-Version">>, Event#edr_event.app_version}
            ,{<<"Body">>, Event#edr_event.body}
            ,{<<"ID">>, Event#edr_event.id}
            ,{<<"Node">>, Event#edr_event.node}
            ,{<<"Severity">>, kz_term:to_binary(Event#edr_event.severity)}
            ,{<<"Timestamp">>, Event#edr_event.timestamp}
            ,{<<"Gregorian-Time">>, Event#edr_event.gregorian_time}
            ,{<<"Verbosity">>, kz_term:to_binary(Event#edr_event.verbosity)}
            ],
    kz_json:from_list(Props);
to_jobj(Opts, Event, IncludeMeta, 'true') ->
    kz_json:normalize(to_jobj(Opts, Event, IncludeMeta, 'false')).

-spec encode(kz_json:object(), kz_json:object()) -> kz_term:ne_binary().
encode(Opts, JObj) ->
    case kz_json:is_true(<<"pretty">>, Opts, 'false') of
        'true' -> kz_json:encode(JObj, ['pretty']);
        'false' -> kz_json:encode(JObj)
    end.
