%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handle channel move logic
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_channel_move).

-export([move/3]).

-include("ecallmgr.hrl").

move(UUID, ONode, NNode) ->
    OriginalNode = wh_util:to_atom(ONode),
    NewNode = wh_util:to_atom(NNode),

    ecallmgr_fs_channel:set_node(NewNode, UUID),
    ecallmgr_call_events:shutdown(OriginalNode, UUID),
    ecallmgr_call_control:update_node(NewNode, UUID),

    lager:debug("updated ~s to point to ~s", [UUID, NewNode]),

    case teardown_sbd(UUID, OriginalNode) of
        'true' ->
            lager:debug("sbd teardown of ~s on ~s", [UUID, OriginalNode]),
            resume(UUID, NewNode);
        'false' ->
            lager:debug("failed to teardown ~s on ~s", [UUID, OriginalNode]),
            'false'
    end.

%% listens for the event from FS with the XML
-spec resume(ne_binary(), atom()) -> boolean().
-spec resume(ne_binary(), atom(), wh_proplist()) -> boolean().
resume(UUID, NewNode) ->
    catch gproc:reg({p, l, ?CHANNEL_MOVE_REG(NewNode, UUID)}),
    lager:debug("waiting for message with metadata for channel ~s so we can move it to ~s", [UUID, NewNode]),
    receive
        ?CHANNEL_MOVE_RELEASED_MSG(_Node, UUID, Evt) ->
            lager:debug("channel has been released from former node: ~s", [_Node]),
            case resume(UUID, NewNode, Evt) of
                'true' -> wait_for_completion(UUID, NewNode);
                'false' -> 'false'
            end
    after 5000 ->
            lager:debug("timed out waiting for channel to be released"),
            'false'
    end.

resume(UUID, NewNode, Evt) ->
    Meta = fix_metadata(props:get_value(<<"metadata">>, Evt)),

    case freeswitch:sendevent_custom(NewNode, ?CHANNEL_MOVE_REQUEST_EVENT
                                     ,[{"profile_name", wh_util:to_list(?DEFAULT_FS_PROFILE)}
                                       ,{"channel_id", wh_util:to_list(UUID)}
                                       ,{"metadata", wh_util:to_list(Meta)}
                                       ,{"technology", wh_util:to_list(props:get_value(<<"technology">>, Evt, <<"sofia">>))}
                                      ]) of
        'ok' ->
            lager:debug("sent channel_move::move_request with metadata to ~s for ~s", [NewNode, UUID]),
            'true';
        {'error', _E} ->
            lager:debug("failed to send custom event channel_move::move_request: ~p", [_E]),
            'false';
        'timeout' ->
            lager:debug("timed out sending custom event channel_move::move_request"),
            'false'
    end.

%% We receive un-escaped < and > in the SIP URIs in this data
%% which causes the XML to not be parsable, either in Erlang or
%% by FreeSWITCH's parser. Things like:
%% <sip_uri><sip:user@realm:port>;tag=abc</sip_uri>
%% So this is an awesome search/replace list to convert the '<sip:'
%% and its corresponding '>' to %3C and %3E as they should be
fix_metadata(Meta) ->
    Replacements = [
                    {<<"\<sip\:">>, <<"%3Csip:">>}
                    ,{<<"\>\<sip">>, <<"%3E<sip">>}
                    ,{<<"\>;">>, <<"%3E;">>} % this is especially nice :)
                    %% until such time as FS sets these properly
                    ,{<<"<dialplan></dialplan>">>, <<"<dialplan>XML</dialplan>">>}
                    ,{<<"<context>default</context>">>, <<"<context>context_2</context>">>}
                   ],
    lists:foldl(fun({S, R}, MetaAcc) ->
                        iolist_to_binary(re:replace(MetaAcc, S, R, ['global']))
                end, Meta, Replacements).

wait_for_completion(UUID, NewNode) ->
    lager:debug("waiting for confirmation from ~s of move", [NewNode]),
    receive
        ?CHANNEL_MOVE_COMPLETE_MSG(_Node, UUID, _Evt) ->
            lager:debug("confirmation of move received for ~s, success!", [_Node]),
            _ = ecallmgr_call_sup:start_event_process(NewNode, UUID),
            'true'
    after 5000 ->
            lager:debug("timed out waiting for move to complete"),
            'false'
    end.

teardown_sbd(UUID, OriginalNode) ->
    catch gproc:reg({'p', 'l', ?CHANNEL_MOVE_REG(OriginalNode, UUID)}),

    case freeswitch:sendevent_custom(OriginalNode, ?CHANNEL_MOVE_REQUEST_EVENT
                                     ,[{"profile_name", wh_util:to_list(?DEFAULT_FS_PROFILE)}
                                       ,{"channel_id", wh_util:to_list(UUID)}
                                       ,{"technology", ?DEFAULT_FS_TECHNOLOGY}
                                      ])
    of
        'ok' ->
            lager:debug("sent channel_move::move_request to ~s for ~s", [OriginalNode, UUID]),
            'true';
        {'error', _E} ->
            lager:debug("failed to send custom event channel_move::move_request: ~p", [_E]),
            'false';
        'timeout' ->
            lager:debug("timed out sending custom event channel_move::move_request"),
            'false'
    end.
