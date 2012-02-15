%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Execute conference commands
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------

-module(ecallmgr_conference_command).

-export([exec_cmd/4]).

-include("ecallmgr.hrl").

-spec exec_cmd/4 :: (atom(), ne_binary(), wh_json:json_object(), pid()) -> 'ok' | 'timeout' | {'error', 'bad_reply' | string()}.
exec_cmd(Node, CallId, JObj, _) ->
    undefined.

%% return the app name and data (as a binary string) to send to the FS ESL via mod_erlang_event
-spec get_fs_app/4 :: (atom(), ne_binary(), wh_json:json_object(), ne_binary()) -> ne_binary() | {'error', string()}.
get_fs_app(_Node, _ConfName, JObj, <<"deaf_participant">>) ->
    case wapi_conference:deaf_participant_v(JObj) of
        false ->
            {'error', <<"conference deaf failed to execute as JObj did not validate.">>};
        true ->
            {<<"deaf">>, wh_json:get_binary_value(<<"Participant">>, JObj)}
    end;
get_fs_app(_Node, _ConfName, JObj, <<"participant_energy">>) ->
    case wh_api:participant_energy_v(JObj) of
        false ->
            {'error', <<"conference deaf failed to execute as JObj did not validate.">>};
        true ->
            Args = list_to_binary([wh_json:get_binary_value(<<"Participant">>, JObj)
                                   ," ", wh_json:get_binary_value(<<"Energy-Level">>, JObj, <<"20">>)
                                  ]),
            {<<"energy">>, Args}
    end;
get_fs_app(_Node, _ConfName, JObj, <<"kick">>) ->
    case wh_api:participant_energy_v(JObj) of
        false ->
            {'error', <<"conference kick failed to execute as JObj did not validate.">>};
        true ->
            {<<"hup">>, wh_json:get_binary_value(<<"Participant">>, JObj, <<"last">>)}
    end;
get_fs_app(_Node, _ConfName, JObj, <<"list">>) ->
    case wh_api:participant_energy_v(JObj) of
        false ->
            {'error', <<"conference list failed to execute as JObj did not validate.">>};
        true ->
            {<<"list">>, wh_json:get_binary_value(<<"Participant">>, JObj, <<"last">>)}
    end.
