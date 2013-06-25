%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_manual_presence).

-export([handle/2]).

-include("../callflow.hrl").

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = whapps_call:kvs_fetch(cf_capture_group, Call),
    PresenceId = case binary:match((P = wh_json:get_binary_value(<<"presence_id">>, Data, CaptureGroup)), <<"@">>) of
                     nomatch -> <<P/binary, "@", (whapps_call:request_realm(Call))/binary>>;
                     _Else -> P
                 end,
    Status = wh_json:get_value(<<"status">>, Data, <<"idle">>),
    update_presence(Status, PresenceId, Call),
    cf_exe:continue(Call).

-spec update_presence(ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
update_presence(<<"idle">>, PresenceId, Call) ->
    _ = couch_mgr:update_doc(whapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, [{PresenceId, <<"terminated">>}]),
    whapps_call_command:presence(<<"terminated">>, PresenceId, wh_util:to_hex_binary(crypto:md5(PresenceId))),
    ok;
update_presence(<<"ringing">>, PresenceId, Call) ->
    whapps_call_command:presence(<<"early">>, PresenceId, wh_util:to_hex_binary(crypto:md5(PresenceId))),
    _ = couch_mgr:update_doc(whapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, [{PresenceId, <<"early">>}]),
    ok;
update_presence(<<"busy">>, PresenceId, Call) ->
    whapps_call_command:presence(<<"confirmed">>, PresenceId, wh_util:to_hex_binary(crypto:md5(PresenceId))),
    _ = couch_mgr:update_doc(whapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, [{PresenceId, <<"confirmed">>}]),
    ok.
