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

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".dynamic_cid">>).

-record(prompts, {accept_tone =
                      whapps_config:get_binary(?MOD_CONFIG_CAT, <<"accept_prompt">>, <<"tone_stream://%(250,50,440)">>),
                  reject_tone =
                      whapps_config:get_binary(?MOD_CONFIG_CAT, <<"reject_prompt">>, <<"/system_media/dynamic-cid-invalid_using_default">>),
                  default_prompt =
                      whapps_config:get_binary(?MOD_CONFIG_CAT, <<"default_prompt">>, <<"/system_media/dynamic-cid-enter_cid">>)
                 }).

-record(dynamic_cid, {
          prompts = #prompts{},
          default_cid = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"default_cid">>, <<"0000000000">>),
          max_digits = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"max_digits">>, <<"10">>),
          min_digits = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"min_digits">>, <<"10">>),
          whitelist = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"whitelist_regex">>, <<"\\d+">>)
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
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    DynamicCID = #dynamic_cid{},
    Prompts = DynamicCID#dynamic_cid.prompts,
    _ = whapps_call_command:b_play(<<"silence_stream://100">>, Call),
    Media = case wh_json:get_ne_value(<<"media_id">>, Data) of
                'undefined' -> Prompts#prompts.default_prompt;
                Else -> Else
            end,
    Min = DynamicCID#dynamic_cid.min_digits,
    Max = DynamicCID#dynamic_cid.max_digits,
    Regex = DynamicCID#dynamic_cid.whitelist,
    DefaultCID = DynamicCID#dynamic_cid.default_cid,
    CID = case whapps_call_command:b_play_and_collect_digits(Min, Max, Media, <<"1">>, <<"5000">>, 'undefined', Regex, Call) of
              {'ok', <<>>} ->
                  _ = whapps_call_command:play(Prompts#prompts.reject_tone, Call),
                  DefaultCID;
              {'ok', Digits} ->
                  _ = whapps_call_command:play(Prompts#prompts.accept_tone, Call),
                  Digits;
              {'error', _} ->
                  _ = whapps_call_command:play(Prompts#prompts.reject_tone, Call),
                  DefaultCID
          end,
    lager:info("setting the caller id number to ~s", [CID]),

    {'ok', C1} = cf_exe:get_call(Call),
    Updates = [fun(C) -> whapps_call:kvs_store('dynamic_cid', CID, C) end
               ,fun(C) -> whapps_call:set_caller_id_number(CID, C) end
              ],
    cf_exe:set_call(whapps_call:exec(Updates, C1)),
    cf_exe:continue(Call).
