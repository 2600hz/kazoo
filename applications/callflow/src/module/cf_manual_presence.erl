%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "presence_id":"foo" // for "foo@bar.com"
%%%   ,"status":"idle" //"idle", "ringing", "busy"
%%% }
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_manual_presence).

-export([handle/2]).

-include("callflow.hrl").

-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = kapps_call:kvs_fetch('cf_capture_group', Call),
    PresenceId =
        case binary:match((P = kz_device:presence_id(Data, CaptureGroup)), <<"@">>) of
            'nomatch' -> <<P/binary, "@", (kapps_call:request_realm(Call))/binary>>;
            _Else -> P
        end,
    Status = kz_json:get_value(<<"status">>, Data, <<"idle">>),
    update_presence(Status, PresenceId, Call),
    cf_exe:continue(Call).

-spec update_presence(ne_binary(), ne_binary(), kapps_call:call()) -> 'ok'.
update_presence(<<"idle">>, PresenceId, Call) ->
    _ = kz_datamgr:update_doc(kapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, [{PresenceId, <<"terminated">>}]),
    kapps_call_command:presence(<<"terminated">>, PresenceId, kz_util:to_hex_binary(crypto:hash(md5, PresenceId)));
update_presence(<<"ringing">>, PresenceId, Call) ->
    _ = kz_datamgr:update_doc(kapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, [{PresenceId, <<"early">>}]),
    kapps_call_command:presence(<<"early">>, PresenceId, kz_util:to_hex_binary(crypto:hash(md5, PresenceId)));
update_presence(<<"busy">>, PresenceId, Call) ->
    _ = kz_datamgr:update_doc(kapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, [{PresenceId, <<"confirmed">>}]),
    kapps_call_command:presence(<<"confirmed">>, PresenceId, kz_util:to_hex_binary(crypto:hash(md5, PresenceId))).
