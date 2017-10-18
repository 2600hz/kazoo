%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
%%% @doc
%%% Receives STOP RECORD event
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_recordings).

-export([init/0]).
-export([handle_record_stop/1]).

-include("ecallmgr.hrl").

-spec init() -> 'ok'.
init() ->
    kazoo_bindings:bind(<<"event_stream.event.call_event.RECORD_STOP">>, ?MODULE, 'handle_record_stop'),
    'ok'.

-spec handle_record_stop(map()) -> any().
handle_record_stop(#{node := Node, call_id := UUID, payload := JObj}) ->
    kz_util:put_callid(UUID),
    IsLocal = handling_locally(JObj),
    MediaRecorder = kz_call_event:custom_channel_var(JObj, <<"Media-Recorder">>),
    maybe_store_recording(IsLocal, MediaRecorder, JObj, UUID, Node).

-spec maybe_store_recording(boolean(), api_binary(), kz_proplist(), ne_binary(), atom()) ->
                                   'ok' |
                                   'error' |
                                   ecallmgr_util:send_cmd_ret() |
                                   [ecallmgr_util:send_cmd_ret(),...].
maybe_store_recording('false', _, _JObj, _CallId, _Node) -> 'ok';
maybe_store_recording('true', <<"kz_media_recording">>, _Props, _CallId, _Node) -> 'ok';
maybe_store_recording('true', _, JObj, CallId, Node) ->
    case kz_call_event:custom_channel_var(JObj, <<"Media-Transfer-Destination">>) of
        'undefined' -> 'ok';
        <<>> -> 'ok';
        <<_/binary>> = Destination ->
            kz_util:put_callid(CallId),
            lager:debug("no one is handling call recording, storing recording to ~s", [Destination]),

            MediaName = kz_call_event:custom_channel_var(JObj, <<"Media-Name">>),
            %% TODO: if you change this logic be sure it matches kz_media_util as well!
            Url = kz_binary:join([kz_binary:strip_right(Destination, $/)
                                 ,MediaName
                                 ]
                                ,<<"/">>
                                ),

            JObj = kz_json:from_list(
                     [{<<"Call-ID">>, CallId}
                     ,{<<"Msg-ID">>, CallId}
                     ,{<<"Media-Name">>, MediaName}
                     ,{<<"Media-Transfer-Destination">>, Url}
                     ,{<<"Insert-At">>, <<"now">>}
                     ,{<<"Media-Transfer-Method">>, media_transfer_method(JObj)}
                     ,{<<"Application-Name">>, <<"store">>}
                     ,{<<"Event-Category">>, <<"call">>}
                     ,{<<"Event-Name">>, <<"command">>}
                      | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                     ]),
            ecallmgr_call_command:exec_cmd(Node, CallId, JObj, 'undefined')
    end.

-spec media_transfer_method(kz_proplist()) -> ne_binary().
media_transfer_method(JObj) ->
    kz_call_event:custom_channel_var(JObj, <<"Media-Transfer-Method">>, <<"put">>).

-spec handling_locally(kz_proplist()) -> boolean().
handling_locally(JObj) ->
    handling_locally(JObj, kz_call_event:other_leg_call_id(JObj)).

-spec handling_locally(kz_json:object(), api_binary()) -> boolean().
handling_locally(JObj, 'undefined') ->
    kz_call_event:custom_channel_var(JObj, <<"Ecallmgr-Node">>) =:= kz_term:to_binary(node());
handling_locally(JObj, OtherLeg) ->
    Node = kz_term:to_binary(node()),
    case kz_call_event:custom_channel_var(JObj, <<"Ecallmgr-Node">>) of
        Node -> 'true';
        _ -> other_leg_handling_locally(OtherLeg)
    end.

-spec other_leg_handling_locally(ne_binary()) -> boolean().
other_leg_handling_locally(OtherLeg) ->
    case ecallmgr_fs_channel:fetch(OtherLeg, 'record') of
        {'ok', #channel{handling_locally=HandleLocally}} -> HandleLocally;
        _ -> 'false'
    end.
