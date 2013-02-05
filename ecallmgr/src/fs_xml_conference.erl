%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Convert FS conference xml to #conference{}
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fs_xml_conference).

-export([xml_to_conference/2]).

-include("ecallmgr.hrl").

-spec xml_to_conference(xml_el() | xml_attribs(), atom()) -> conference().
xml_to_conference(#xmlElement{name='conference'
                              ,attributes=Attrs
                             }, Node) ->
    xml_attrs_to_record(Attrs, Node);
xml_to_conference([#xmlAttribute{}|_]=Attrs, Node) ->
    xml_attrs_to_record(Attrs, Node).

xml_attrs_to_record(Attrs, Node) ->
    update_record_with_xml_attrs(Attrs, #conference{node=Node}).

update_record_with_xml_attrs([], Conf) -> Conf;
update_record_with_xml_attrs([#xmlAttribute{name=N, value=V}|Attrs], Conf) ->
    update_record_with_xml_attrs(Attrs, update_record(Conf, N, V)).

update_record(Conf, 'name', V) ->
    Conf#conference{name=wh_util:to_binary(V)};
update_record(Conf, 'member-count', V) ->
    Conf#conference{participants=wh_util:to_integer(V)};
update_record(Conf, 'uuid', V) ->
    Conf#conference{uuid=wh_util:to_binary(V)};
update_record(Conf, 'running', V) ->
    Conf#conference{running=wh_util:is_true(V)};
update_record(Conf, 'answered', V) ->
    Conf#conference{answered=wh_util:is_true(V)};
update_record(Conf, 'enforce_min', V) ->
    Conf#conference{enforce_min=wh_util:is_true(V)};
update_record(Conf, 'dynamic', V) ->
    Conf#conference{dynamic=wh_util:is_true(V)};
update_record(Conf, 'exit_sound', V) ->
    Conf#conference{exit_sound=wh_util:is_true(V)};
update_record(Conf, 'enter_sound', V) ->
    Conf#conference{enter_sound=wh_util:is_true(V)};
update_record(Conf, 'run_time', V) ->
    Conf#conference{run_time=wh_util:to_integer(V)};
update_record(Conf, _K, _V) ->
    lager:debug("unhandled conference k/v ~s: ~p", [_K, _V]),
    Conf.

    
