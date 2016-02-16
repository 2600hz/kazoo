%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
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

-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    CaptureGroup = whapps_call:kvs_fetch('cf_capture_group', Call),
    PresenceId =
        case binary:match((P = kz_device:presence_id(Data, CaptureGroup)), <<"@">>) of
            'nomatch' -> <<P/binary, "@", (whapps_call:request_realm(Call))/binary>>;
            _Else -> P
        end,
    Status = wh_json:get_value(<<"status">>, Data, <<"idle">>),
    update_presence(Status, PresenceId, Call),
    cf_exe:continue(Call).

-spec update_presence(ne_binary(), ne_binary(), whapps_call:call()) -> 'ok'.
update_presence(<<"idle">>, PresenceId, Call) ->
    _ = kz_datamgr:update_doc(whapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, [{PresenceId, <<"terminated">>}]),
    whapps_call_command:presence(<<"terminated">>, PresenceId, wh_util:to_hex_binary(crypto:hash(md5, PresenceId)));
update_presence(<<"ringing">>, PresenceId, Call) ->
    _ = kz_datamgr:update_doc(whapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, [{PresenceId, <<"early">>}]),
    whapps_call_command:presence(<<"early">>, PresenceId, wh_util:to_hex_binary(crypto:hash(md5, PresenceId)));
update_presence(<<"busy">>, PresenceId, Call) ->
    _ = kz_datamgr:update_doc(whapps_call:account_db(Call), ?MANUAL_PRESENCE_DOC, [{PresenceId, <<"confirmed">>}]),
    whapps_call_command:presence(<<"confirmed">>, PresenceId, wh_util:to_hex_binary(crypto:hash(md5, PresenceId))).
