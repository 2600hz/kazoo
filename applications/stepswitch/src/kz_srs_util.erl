%%%-------------------------------------------------------------------
%%% @copyright
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(kz_srs_util).

-export([select_filter_action/1, select_filter_mode/3]).
-export([get_source/1, get_value/5, get_value/6]).
-export([check_value/2]).

-include("stepswitch_resource_selectors.hrl").

select_filter_action(Params) ->
    do_select_filter_action(kz_json:get_ne_binary_value(<<"action">>, Params)).
do_select_filter_action(<<"keep">>) -> 'keep';
do_select_filter_action(<<"drop">>) -> 'drop';
do_select_filter_action(Data) -> throw({invalid_filter_action, Data}).

select_filter_mode(Params, ModesList, Default) ->
    do_select_filter_mode(kz_json:get_ne_binary_value(<<"mode">>, Params, Default), ModesList).
do_select_filter_mode(Mode, ModesList) ->
    case lists:member(Mode, ModesList) of
        'true' -> Mode;
        'false' -> throw({invalid_filter_mode, Mode})
    end.

get_source(<<"number">>) -> 'number';
get_source(<<"cid_number">>) -> 'cid_number';
get_source(<<"request:", Field/binary>>) -> {'request', Field};
get_source(<<"resource:", Field/binary>>) -> {'resource', Field};
get_source(<<"database:", Selector/binary>>) -> {'database', Selector};
get_source(Type) -> throw({invalid_filter_type, Type}).

get_value(Type, Resources, Number, OffnetJObj, DB) ->
    get_value(Type, Resources, Number, OffnetJObj, DB, 'undefined').
get_value('number', _Resources, Number, _OffnetJObj, _DB, _Default) -> Number;
get_value('cid_number', _Resources, _Number, OffnetJObj, _DB, _Default) ->
    case ?RULES_HONOR_DIVERSION of
        'false' -> kapi_offnet_resource:outbound_caller_id_number(OffnetJObj);
        'true' -> stepswitch_resources:check_diversion_fields(OffnetJObj)
    end;
get_value({'request', Field}, _Resources, _Number, OffnetJObj, _DB, Default) ->
    kz_json:get_value(Field, OffnetJObj, Default);
get_value({'resource', Field}, Resources, _Number, _OffnetJObj, _DB, _Default) ->
    [{stepswitch_resources:get_resrc_id(R), get_data_from_resource(Field, R)}
     || R <- Resources];
get_value({'database', SelectorName}, Resources, Number, OffnetJObj, DB, Default) ->
    Keys = [[stepswitch_resources:get_resrc_id(R), SelectorName] || R <- Resources],
    View = <<"resource_selectors/resource_name_listsing">>,
    Options = [{'keys', Keys}],
    get_value({'database', View, Options}, Resources, Number, OffnetJObj, DB, Default);
get_value({'database', View, Options}, _Resources, _Number, _OffnetJObj, DB, Default) ->
    case kz_datamgr:get_results(DB, View, Options) of
        {'ok', Rows} ->
            lists:foldl(fun(Row, Acc) ->
                                ID = kz_json:get_ne_value([<<"value">>, <<"resource">>], Row),
                                Value = kz_json:get_ne_value([<<"value">>, <<"data">>], Row),
                                OldValue = props:get_value(ID, Acc, Default),
                                props:set_value(ID, [Value | OldValue], Acc)
                        end
                        ,[]
                        ,Rows
                       );
        {'error', E} ->
            throw({database_error, E})
    end;
get_value(Value, _Resources, _Number, _OffnetJObj, _DB, _Default) -> throw({invalid_filter_value, Value}).

get_data_from_resource(ResourceField, Resource) ->
    case props:get_value(ResourceField, ?ALLOWED_RESOURCE_FIELDS) of
        'undefined' -> throw({forbidden_resource_field, ResourceField});
        Field -> stepswitch_resources:Field(Resource)
    end.

check_value(Fun, Value) ->
    case Fun(Value) of
        'true' -> 'ok';
        'false' -> throw({invalid_value_type, Value})
    end.
