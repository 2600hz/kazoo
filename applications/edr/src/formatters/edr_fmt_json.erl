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

-spec format_event(kz_json:object(), event()) -> ne_binary().
-spec format_event(kz_json:object(), event(), boolean()) -> ne_binary().
-spec format_event(kz_json:object(), event(), boolean(), boolean()) -> ne_binary().
format_event(Opts, Event) ->
    JObj = format_event(Opts, Event, props:get_is_true(<<"include_metadata">>, Opts, 'true')),
    kz_json:encode(JObj).
format_event(_Opts, #event{body=JObj}, 'false') ->
    kz_json:encode(JObj);
format_event(Opts, Event, 'true') ->
    format_event(Opts, Event, 'true', kz_json:get_value(<<"normalize">>, Opts, 'true')).
format_event(_Opts, Event, _IncludeMeta, 'false') ->
    Props = [{<<"account_id">>, Event#event.account_id}
            ,{<<"account_tree">>, Event#event.account_tree}
            ,{<<"app_name">>, Event#event.app_name}
            ,{<<"app_version">>, Event#event.app_version}
            ,{<<"body">>, Event#event.body}
            ,{<<"level">>, kz_term:to_binary(Event#event.level)}
            ,{<<"timestamp">>, Event#event.timestamp}
            ],
    kz_json:from_list(Props);
format_event(Opts, Event, IncludeMeta, 'true') ->
    kz_json:normalize(format_event(Opts, Event, IncludeMeta, 'false')).
