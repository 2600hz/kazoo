%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, Conversant Ltd
%%% @doc
%%% JSON formatter module
%%% @end
%%% @contributors
%%%    Max Lay
%%%-------------------------------------------------------------------
-module(edr_fmt_json).

-export([format_event/2]).

-behaviour(edr_formatter).

-include("../edr.hrl").

-type result() :: ne_binary() | kz_json:object().

-spec format_event(kz_json:object(), edr_event()) -> result().
-spec format_event(kz_json:object(), edr_event(), boolean()) -> result().
-spec format_event(kz_json:object(), edr_event(), boolean(), boolean()) -> result().
format_event(Opts, Event) ->
    JObj = format_event(Opts, Event, kz_json:is_true(<<"include_metadata">>, Opts, 'true')),
    encode(Opts, JObj).
format_event(Opts, #edr_event{body=JObj}, 'false') ->
    encode(Opts, JObj);
format_event(Opts, Event, 'true') ->
    format_event(Opts, Event, 'true', kz_json:get_value(<<"normalize">>, Opts, 'true')).
format_event(_Opts, Event, _IncludeMeta, 'false') ->
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
format_event(Opts, Event, IncludeMeta, 'true') ->
    kz_json:normalize(format_event(Opts, Event, IncludeMeta, 'false')).

-spec encode(kz_json:object(), kz_json:object()) -> result().
-spec encode(kz_json:object(), kz_json:object(), boolean()) -> result().
encode(Opts, JObj) ->
    encode(Opts, JObj, kz_json:is_true(<<"encode">>, Opts, 'true')).
encode(Opts, JObj, 'true') ->
    case kz_json:is_true(<<"pretty">>, Opts, 'false') of
        'true' -> kz_json:encode(JObj, ['pretty']);
        'false' -> kz_json:encode(JObj)
    end;
encode(_Opts, JObj, 'false') ->
    JObj.
