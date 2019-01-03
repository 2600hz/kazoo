%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handle channel move logic
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_channel_move).

-export([move/3]).

-include("ecallmgr.hrl").

-spec move(kz_term:ne_binary(), atom(), atom()) -> boolean().
move(UUID, ONode, NNode) ->
    OriginalNode = kz_term:to_atom(ONode),
    NewNode = kz_term:to_atom(NNode),

    'true' = ecallmgr_fs_nodes:has_capability(OriginalNode, <<"channel_move">>)
        andalso ecallmgr_fs_nodes:has_capability(NewNode, <<"channel_move">>),
    lager:debug("both servers support channel_move, continuing"),

    ecallmgr_fs_channel:set_node(NewNode, UUID),
    ecallmgr_call_events:shutdown(OriginalNode, UUID),
    ecallmgr_call_control:update_node(NewNode, UUID),

    lager:debug("updated ~s to point to ~s", [UUID, NewNode]),

    _ = teardown_sbd(UUID, OriginalNode),
    case wait_for_teardown(UUID, OriginalNode) of
        {'ok', Evt} -> rebuild_channel(UUID, NewNode, Evt);
        {'error', 'timeout'} -> 'false'
    end.

%% listens for the event from FS with the XML
-spec wait_for_teardown(kz_term:ne_binary(), atom()) ->
                               {'ok', kzd_freeswitch:data()} |
                               {'error', 'timeout'}.
wait_for_teardown(UUID, OriginalNode) ->
    receive
        ?CHANNEL_MOVE_RELEASED_MSG(OriginalNode, UUID, Evt) ->
            lager:debug("channel has been released from former node: ~s", [OriginalNode]),
            {'ok', Evt};
        ?CHANNEL_MOVE_RELEASED_MSG(_Node, UUID, _Evt) ->
            lager:debug("recv move_released for node ~s, expecting ~s", [_Node, OriginalNode]),
            wait_for_teardown(UUID, OriginalNode)
    after 5 * ?MILLISECONDS_IN_SECOND ->
            {'error', 'timeout'}
    end.

-spec rebuild_channel(kz_term:ne_binary(), atom(), kz_term:proplist()) -> boolean().
rebuild_channel(UUID, NewNode, Evt) ->
    catch gproc:reg({'p', 'l', ?CHANNEL_MOVE_REG(NewNode, UUID)}),
    lager:debug("waiting for message with metadata for channel ~s so we can move it to ~s", [UUID, NewNode]),
    resume(UUID, NewNode, Evt),
    wait_for_completion(UUID, NewNode).

-spec resume(kz_term:ne_binary(), atom(), kz_term:proplist()) -> 'ok'.
resume(UUID, NewNode, Evt) ->
    Meta = fix_metadata(props:get_value(<<"metadata">>, Evt)),

    freeswitch:sendevent_custom(NewNode, ?CHANNEL_MOVE_REQUEST_EVENT
                               ,[{"profile_name", kz_term:to_list(?DEFAULT_FS_PROFILE)}
                                ,{"channel_id", kz_term:to_list(UUID)}
                                ,{"metadata", kz_term:to_list(Meta)}
                                ,{"technology", kz_term:to_list(props:get_value(<<"technology">>, Evt, <<"sofia">>))}
                                ]),
    lager:debug("sent channel_move::move_request with metadata to ~s for ~s", [NewNode, UUID]).

%% We receive un-escaped < and > in the SIP URIs in this data
%% which causes the XML to not be parsable, either in Erlang or
%% by FreeSWITCH's parser. Things like:
%% <sip_uri><sip:user@realm:port>;tag=abc</sip_uri>
%% So this is an awesome search/replace list to convert the '<sip:'
%% and its corresponding '>' to %3C and %3E as they should be
-spec fix_metadata(kz_term:ne_binary()) -> kz_term:ne_binary().
fix_metadata(Meta) ->
    Replacements = [{<<"<sip:">>, <<"%3Csip:">>}
                   ,{<<"><sip">>, <<"%3E<sip">>}
                   ,{<<">;">>, <<"%3E;">>} % this is especially nice :)
                    %% until such time as FS sets these properly
                   ,{<<"<dialplan></dialplan>">>, <<"<dialplan>XML</dialplan>">>}
                   ,{<<"<context>default</context>">>, <<"<context>context_2</context>">>}
                   ],
    lists:foldl(fun({S, R}, MetaAcc) ->
                        iolist_to_binary(re:replace(MetaAcc, S, R, ['global']))
                end, Meta, Replacements).

-spec wait_for_completion(kz_term:ne_binary(), atom()) -> boolean().
wait_for_completion(UUID, NewNode) ->
    lager:debug("waiting for confirmation from ~s of move", [NewNode]),
    receive
        ?CHANNEL_MOVE_COMPLETE_MSG(NewNode, UUID, _Evt) ->
            lager:debug("confirmation of move received for ~s, success!", [NewNode]),
            _ = ecallmgr_call_sup:start_event_process(NewNode, UUID),
            'true';
        ?CHANNEL_MOVE_COMPLETE_MSG(_Node, _UUID, _Evt) ->
            lager:debug("recv move_complete from unexpected node ~s: ~s", [_Node, _UUID]),
            wait_for_completion(UUID, NewNode)
    after 5 * ?MILLISECONDS_IN_SECOND ->
            lager:debug("timed out waiting for move to complete"),
            'false'
    end.

-spec teardown_sbd(kz_term:ne_binary(), atom()) -> 'ok'.
teardown_sbd(UUID, OriginalNode) ->
    catch gproc:reg({'p', 'l', ?CHANNEL_MOVE_REG(OriginalNode, UUID)}),

    freeswitch:sendevent_custom(OriginalNode, ?CHANNEL_MOVE_REQUEST_EVENT
                               ,[{"profile_name", kz_term:to_list(?DEFAULT_FS_PROFILE)}
                                ,{"channel_id", kz_term:to_list(UUID)}
                                ,{"technology", ?DEFAULT_FS_TECHNOLOGY}
                                ]),
    lager:debug("sent channel_move::move_request to ~s for ~s", [OriginalNode, UUID]).
