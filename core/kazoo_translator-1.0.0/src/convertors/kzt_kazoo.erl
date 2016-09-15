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

-include("../kzt.hrl").

-spec exec(whapps_call:call(), wh_json:object()) -> usurp_return() | {'error', whapps_call:call()}.
exec(Call, FlowJObj) ->
    %% rudimentary sanity check, to avoid crashing in amqp handler
    case wh_json:get_value(<<"module">>, FlowJObj) of
        'undefined' -> {'error', Call};
        _ -> unsafe_exec(Call, FlowJObj)
    end.

-spec unsafe_exec(whapps_call:call(), wh_json:object()) -> usurp_return().
unsafe_exec(Call, FlowJObj) ->
    Prop = [{<<"Call">>, whapps_call:to_json(Call)}
            ,{<<"Flow">>, FlowJObj}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    wapi_callflow:publish_resume(Prop),
    {'usurp', Call}.

-spec parse_cmds(ne_binary()) ->
                        {'ok', wh_json:object()} |
                        {'error', 'not_parsed'}.
parse_cmds(JSON) ->
    try wh_json:decode(JSON) of
        JObj -> {'ok', JObj}
    catch
        _E:_R ->
            lager:debug("failed to process json: ~s: ~p", [_E, _R]),
            {'error', 'not_parsed'}
    end.

-spec req_params(whapps_call:call()) -> wh_proplist().
req_params(Call) ->
    Owners = case cf_attributes:owner_ids(whapps_call:authorizing_id(Call), Call) of
                 [] -> 'undefined';
                 [OwnerId] -> OwnerId;
                 [_|_]=IDs -> IDs
             end,
    props:filter_undefined(
      [{<<"Call-ID">>, whapps_call:call_id(Call)}
       ,{<<"Account-ID">>, whapps_call:account_id(Call)}
       ,{<<"From">>, whapps_call:from_user(Call)}
       ,{<<"From-Realm">>, whapps_call:from_realm(Call)}
       ,{<<"To">>, whapps_call:to_user(Call)}
       ,{<<"To-Realm">>, whapps_call:to_realm(Call)}
       ,{<<"Request">>, whapps_call:request_user(Call)}
       ,{<<"Request-Realm">>, whapps_call:request_realm(Call)}
       ,{<<"Call-Status">>, kzt_util:get_call_status(Call)}
       ,{<<"Api-Version">>, <<"2015-03-01">>}
       ,{<<"Direction">>, <<"inbound">>}
       ,{<<"Caller-ID-Name">>, whapps_call:caller_id_name(Call)}
       ,{<<"Caller-ID-Number">>, whapps_call:caller_id_number(Call)}
       ,{<<"User-ID">>, Owners}
       ,{<<"Language">>, whapps_call:language(Call)}
       ,{<<"Recording-Url">>, kzt_util:get_recording_url(Call)}
       ,{<<"Recording-Duration">>, kzt_util:get_recording_duration(Call)}
       ,{<<"Recording-ID">>, kzt_util:get_recording_sid(Call)}
       ,{<<"Digits">>, kzt_util:get_digit_pressed(Call)}
       ,{<<"Transcription-ID">>, kzt_util:get_transcription_sid(Call)}
       ,{<<"Transcription-Text">>, kzt_util:get_transcription_text(Call)}
       ,{<<"Transcription-Status">>, kzt_util:get_transcription_status(Call)}
       ,{<<"Transcription-Url">>, kzt_util:get_transcription_url(Call)}
      ]).
