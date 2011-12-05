%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 1 Dec 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_dynamic_cid).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (Data, Call) -> {'stop' | 'continue'} when
      Data :: json_object(),
      Call :: #cf_call{}.
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    AcceptTone = whapps_config:get_binary(<<"callflow.dynamic_cid">>, <<"accept_prompt">>, <<"tone_stream://%(250,50,440)">>),
    RejectTone = whapps_config:get_binary(<<"callflow.dynamic_cid">>, <<"reject_prompt">>, <<"/system_media/dynamic-cid-invalid_using_default">>),
    DefaultCID = whapps_config:get_binary(<<"callflow.dynamic_cid">>, <<"default_cid">>, <<"0000000000">>),
    MinDigits = whapps_config:get_binary(<<"callflow.dynamic_cid">>, <<"min_digits">>, <<"10">>),
    MaxDigits = whapps_config:get_binary(<<"callflow.dynamic_cid">>, <<"max_digits">>, <<"25">>),
    WhiteRegex = whapps_config:get_binary(<<"callflow.dynamic_cid">>, <<"whitelist_regex">>, <<"\\d+">>),
    %%    BlackRegex = whapps_config:get(<<"callflow.dynamic_cid">>, <<"blacklist_regex">>),
    DefaultPrompt = whapps_config:get_binary(<<"callflow.dynamic_cid">>, <<"default_prompt">>, <<"/system_media/dynamic-cid-enter_cid">>),
    cf_call_command:b_play("silence_stream://100", Call),
    Media = case wh_json:get_ne_value(<<"media_id">>, Data) of
                undefined ->
                    DefaultPrompt;
                Else ->
                    Else
            end,
    CID = case cf_call_command:b_play_and_collect_digits(MinDigits, MaxDigits, Media, <<"1">>, <<"5000">>, undefined, WhiteRegex, Call) of
              {ok, <<>>} ->
                  cf_call_command:play(RejectTone, Call),
                  DefaultCID;
              {ok, Digits} ->
                  cf_call_command:play(AcceptTone, Call),
                  Digits;
              {error, _} ->
                  cf_call_command:play(RejectTone, Call),
                  DefaultCID
          end,
    ?LOG("setting the caller id number to ~s", [CID]),
    cf_call_command:set(wh_json:from_list([{<<"Caller-ID-Number">> ,CID}]), undefined, Call),
    CFPid ! { continue }.
