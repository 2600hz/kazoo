%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%% Process dynamicly generated callflow "flow"
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kzt_kazoo).

-export([exec/2
         ,parse_cmds/1
         ,req_params/1
        ]).

-include_lib("kazoo_translator/src/kzt.hrl").

-spec exec(kapps_call:call(), kz_json:object()) -> usurp_return().
exec(Call, FlowJObj) ->
    Prop = [{<<"Call">>, kapps_call:to_json(Call)}
            ,{<<"Flow">>, FlowJObj}
            | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    kapi_callflow:publish_resume(Prop),
    {'usurp', Call}.

-spec parse_cmds(ne_binary()) ->
                        {'ok', kz_json:object()} |
                        {'error', 'not_parsed'}.
parse_cmds(JSON) ->
    try kz_json:decode(JSON) of
        JObj -> {'ok', JObj}
    catch
        _E:_R ->
            lager:debug("failed to process json: ~s: ~p", [_E, _R]),
            {'error', 'not_parsed'}
    end.

-spec req_params(kapps_call:call()) -> kz_proplist().
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
       ,{<<"Call-Status">>, kzt_util:get_call_status(Call)}
       ,{<<"Api-Version">>, <<"2015-03-01">>}
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
