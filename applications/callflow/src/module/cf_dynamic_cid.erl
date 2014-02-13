%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% "data":{
%%%   "media_id":"id_of_media"
%%%   // optional after this
%%%   "interdigit_timeout":2000
%%% }
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_dynamic_cid).

-include("../callflow.hrl").

-export([handle/2]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".dynamic_cid">>).

-record(prompts, {accept_tone =
                      whapps_config:get_binary(?MOD_CONFIG_CAT, <<"accept_prompt">>, <<"tone_stream://%(250,50,440)">>),
                  reject_tone =
                      whapps_config:get_binary(?MOD_CONFIG_CAT, <<"reject_prompt">>, <<"/system_media/dynamic-cid-invalid_using_default">>),
                  default_prompt =
                      whapps_config:get_binary(?MOD_CONFIG_CAT, <<"default_prompt">>, <<"/system_media/dynamic-cid-enter_cid">>)
                 }).
-type prompts() :: #prompts{}.

-record(dynamic_cid, {
          prompts = #prompts{} :: prompts(),
          default_cid = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"default_cid">>, <<"0000000000">>),
          max_digits = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"max_digits">>, 10),
          min_digits = whapps_config:get_integer(?MOD_CONFIG_CAT, <<"min_digits">>, 10),
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

    Interdigit = wh_json:get_integer_value(<<"interdigit_timeout">>
                                           ,Data
                                           ,whapps_call_commnd:default_interdigit_timeout()
                                          ),

    NoopId = whapps_call_command:play(Media, Call),

    CID = case whapps_call_command:collect_digits(Max
                                                  ,whapps_call_command:default_collect_timeout()
                                                  ,Interdigit
                                                  ,NoopId
                                                  ,Call
                                                 )
          of
              {'ok', <<>>} ->
                  _ = whapps_call_command:play(Prompts#prompts.reject_tone, Call),
                  DefaultCID;
              {'ok', Digits} ->
                  case re:run(Digits, Regex) of
                      {'match', _} when byte_size(Digits) >= Min ->
                          whapps_call_command:play(Prompts#prompts.accept_tone, Call),
                          Digits;
                      _ ->
                          _ = whapps_call_command:play(Prompts#prompts.reject_tone, Call),
                          DefaultCID
                  end;
              {'error', _} ->
                  _ = whapps_call_command:play(Prompts#prompts.reject_tone, Call),
                  DefaultCID
          end,
    lager:info("setting the caller id number to ~s", [CID]),

    {'ok', C1} = cf_exe:get_call(Call),
    Updates = [{fun whapps_call:kvs_store/3, 'dynamic_cid', CID}
               ,{fun whapps_call:set_caller_id_number/2, CID}
              ],
    cf_exe:set_call(whapps_call:exec(Updates, C1)),
    cf_exe:continue(Call).
