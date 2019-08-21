%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Process dynamically generated callflow "flow"
%%% @author James Aimonetti
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzt_kazoo).

-export([exec/2
        ,parse_cmds/1
        ,req_params/1
        ]).

-include("kzt.hrl").

-spec exec(kapps_call:call(), kz_json:object()) ->
                  usurp_return() |
                  {'error', [jesse_error:error_return()]}.
exec(Call, FlowJObj) ->
    case kzd_callflows:validate_flow(
           kzd_callflows:set_flow(kzd_callflows:new(), FlowJObj)
          )
    of
        {'error', Errors} -> {'error', Call, Errors};
        {'ok', ValidCallflow} ->
            resume_callflow(Call, kzd_callflows:flow(ValidCallflow))
    end.

-spec resume_callflow(kapps_call:call(), kz_json:object()) -> usurp_return().
resume_callflow(Call, FlowJObj) ->
    Event = [{<<"Event-Name">>, <<"CHANNEL_PIVOT">>}
            ,{<<"Application-Data">>, kz_json:encode(FlowJObj)}
            ,{<<"Call-ID">>, kapps_call:call_id_direct(Call)}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ],
    'ok' = kz_amqp_worker:cast(Event, fun kapi_call:publish_event/1),
    {'usurp', Call}.

-spec parse_cmds(kz_term:ne_binary()) ->
                        {'ok', kz_json:object()} |
                        {'error', 'not_parsed'}.
parse_cmds(<<_/binary>> = JSON) ->
    try kz_json:unsafe_decode(JSON) of
        JObj -> {'ok', JObj}
    catch
        'throw':{'invalid_json',{'error',{Char,'invalid_json'}},Bin} ->
            <<Before:Char/binary, After/binary>> = Bin,
            lager:debug("JSON error around char ~p", [Char]),
            lager:debug("before: ~s", [Before]),
            lager:debug("after: ~s", [After]),
            throw({'json', <<"invalid JSON">>, Before, After});
        'throw':{'invalid_json',{'error',{Char,'invalid_trailing_data'}},Bin} ->
            TrailingChar = Char-1,
            <<Before:TrailingChar/binary, After/binary>> = Bin,
            lager:debug("trailing data detected: '~s'", [After]),
            throw({'json', <<"trailing data">>, Before, After});
        _E:_R ->
            lager:debug("failed to process json: ~s: ~p", [_E, _R]),
            {'error', 'not_parsed'}
    end.

-spec req_params(kapps_call:call()) -> kz_term:proplist().
req_params(Call) ->
    Owners = case kz_attributes:owner_ids(kapps_call:authorizing_id(Call), Call) of
                 [] -> 'undefined';
                 [OwnerId] -> OwnerId;
                 [_|_]=IDs -> IDs
             end,
    props:filter_undefined(
      [{<<"Call-ID">>, kapps_call:call_id(Call)}
      ,{<<"Account-ID">>, kapps_call:account_id(Call)}
      ,{<<"From">>, kapps_call:from_user(Call)}
      ,{<<"From-Realm">>, kapps_call:from_realm(Call)}
      ,{<<"To">>, kapps_call:to_user(Call)}
      ,{<<"To-Realm">>, kapps_call:to_realm(Call)}
      ,{<<"Request">>, kapps_call:request_user(Call)}
      ,{<<"Request-Realm">>, kapps_call:request_realm(Call)}
      ,{<<"Call-Status">>, kzt_util:get_call_status(Call)}
      ,{<<"Api-Version">>, <<"4.x">>}
      ,{<<"Direction">>, <<"inbound">>}
      ,{<<"Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
      ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
      ,{<<"User-ID">>, Owners}
      ,{<<"Language">>, kapps_call:language(Call)}
      ,{<<"Recording-Url">>, kzt_util:get_recording_url(Call)}
      ,{<<"Recording-Duration">>, kzt_util:get_recording_duration(Call)}
      ,{<<"Recording-ID">>, kzt_util:get_recording_sid(Call)}
      ,{<<"Digits">>, kzt_util:get_digit_pressed(Call)}
      ,{<<"Transcription-ID">>, kzt_util:get_transcription_sid(Call)}
      ,{<<"Transcription-Text">>, kzt_util:get_transcription_text(Call)}
      ,{<<"Transcription-Status">>, kzt_util:get_transcription_status(Call)}
      ,{<<"Transcription-Url">>, kzt_util:get_transcription_url(Call)}
      ]).
