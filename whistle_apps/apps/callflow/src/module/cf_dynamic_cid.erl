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

-define(GET_CONFIG, fun(Key, Default) ->
                      whapps_config:get_binary(<<"callflow.dynamic_cid">>, Key, Default)
                    end).
                            
-record(prompts, {accept_tone = 
                      ?GET_CONFIG(<<"accept_prompt">>, <<"tone_stream://%(250,50,440)">>),
                  reject_tone =
                      ?GET_CONFIG(<<"reject_prompt">>, <<"/system_media/dynamic-cid-invalid_using_default">>),
                  default_prompt = 
                      ?GET_CONFIG(<<"default_prompt">>, <<"/system_media/dynamic-cid-enter_cid">>)
                 }).

-record(dynamic_cid, {
          prompts = #prompts{},
          default_cid = ?GET_CONFIG(<<"default_cid">>, <<"0000000000">>),
          max_digits = ?GET_CONFIG(<<"max_digits">>, <<"10">>),
          min_digits = ?GET_CONFIG(<<"min_digits">>, <<"10">>),
          whitelist = ?GET_CONFIG(<<"whitelist_regex">>, <<"\\d+">>)
         }).
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or
%% stop when successfull.
%%
%% NOTE: It is written in a strange way to make it easier when Karl can
%%       comeback and make it correctly ;)
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), whapps_call:call()) -> ok.
handle(Data, Call) ->
    DynamicCID = #dynamic_cid{},
    Prompts = DynamicCID#dynamic_cid.prompts,
    cf_call_command:b_play("silence_stream://100", Call),
    Media = case wh_json:get_ne_value(<<"media_id">>, Data) of
                undefined ->
                    Prompts#prompts.default_prompt;
                Else ->
                    Else
            end,
    Min = DynamicCID#dynamic_cid.min_digits,
    Max = DynamicCID#dynamic_cid.max_digits,
    Regex = DynamicCID#dynamic_cid.whitelist, 
    DefaultCID = DynamicCID#dynamic_cid.default_cid,
    CID = case cf_call_command:b_play_and_collect_digits(Min, Max, Media, <<"1">>, <<"5000">>, undefined, Regex, Call) of
              {ok, <<>>} ->
                  cf_call_command:play(Prompts#prompts.reject_tone, Call),
                  DefaultCID;
              {ok, Digits} ->
                  cf_call_command:play(Prompts#prompts.accept_tone, Call),
                  Digits;
              {error, _} ->
                  cf_call_command:play(Prompts#prompts.reject_tone, Call),
                  DefaultCID
          end,
    ?LOG("setting the caller id number to ~s", [CID]),
    cf_call_command:set(wh_json:from_list([{<<"Caller-ID-Number">> ,CID}]), undefined, Call),
    cf_exe:continue(Call).
