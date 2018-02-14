%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, 2600Hz INC
%%% @doc
%%% Base module for callflow action
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_audio_macro).

-behaviour(gen_cf_action).

-export([handle/2]).

-include("callflow.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    NoopId = kapps_call_command:audio_macro(get_macro(Data, Call), Call),
    case cf_util:wait_for_noop(Call, NoopId) of
        {'ok', Call1} -> cf_exe:continue(Call1);
        {'error', 'channel_hungup'} ->
            lager:info("channel hungup waiting for noop ~s", [NoopId]),
            cf_exe:stop(Call);
        {'error', _E} ->
            lager:info("waiting for noop ~s errored: ~p", [NoopId, _E]),
            cf_exe:continue(Call)
    end.

-spec get_macro(kz_json:object(), kapps_call:call()) ->
                       kapps_call_command:audio_macro_prompts().
get_macro(Data, Call) ->
    {Macros, _, _} = lists:foldl(fun get_macro_entry/2
                                ,{[]
                                 ,Data
                                 ,kapps_call:account_id(Call)
                                 }
                                ,kz_json:get_list_value(<<"macros">>, Data, [])
                                ),
    lists:reverse(Macros).

-type acc() :: {kapps_call_command:audio_macro_prompts()
               ,kz_json:object()
               ,kz_term:ne_binary()
               }.

-spec get_macro_entry(kz_json:object(), acc()) -> acc().
get_macro_entry(Macro, Acc) ->
    get_macro_entry(Macro, Acc, kz_json:get_ne_binary_value(<<"macro">>, Macro)).

-spec get_macro_entry(kz_json:object(), acc(), kz_term:ne_binary()) -> acc().
get_macro_entry(Macro, {Macros, Data, AccountId}=Acc, <<"play">>) ->
    Path = kz_json:get_ne_binary_value(<<"id">>, Macro),
    case kz_media_util:media_path(Path, AccountId) of
        'undefined' ->
            lager:info("invalid play command '~p', skipping", [Path]),
            Acc;
        Media ->
            Terminators = kz_json:find(<<"terminators">>, [Macro, Data], ?ANY_DIGIT),

            {[{'play', Media, Terminators} | Macros]
            ,Data
            ,AccountId
            }
    end;
get_macro_entry(Macro, {Macros, Data, AccountId}, <<"prompt">>) ->
    PromptId = kz_json:get_ne_binary_value(<<"id">>, Macro),
    case kz_json:find(<<"language">>, [Macro, Data]) of
        'undefined' ->
            {[{'prompt', PromptId} | Macros], Data, AccountId};
        Language ->
            {[{'prompt', PromptId, Language} | Macros], Data, AccountId}
    end;
get_macro_entry(Macro, {Macros, Data, AccountId}=Acc, <<"say">>) ->
    Say = kz_json:get_ne_binary_value(<<"text">>, Macro),
    Type = kz_json:get_ne_binary_value(<<"type">>, Macro),
    Method = kz_json:get_ne_binary_value(<<"method">>, Macro),
    Language = kz_json:find(<<"language">>, [Macro, Data]),
    Gender = kz_json:get_ne_binary_value(<<"gender">>, Macro),

    case say_macro([Say, Type, Method, Language, Gender]) of
        'undefined' ->
            lager:info("invalid say macro '~p'", [Say]),
            Acc;
        SayMacro ->
            {[SayMacro | Macros], Data, AccountId}
    end;
get_macro_entry(Macro, {Macros, Data, AccountId}=Acc, <<"tts">>) ->
    TTSData = [kz_json:get_binary_value(<<"text">>, Data)
              ,kz_json:get_binary_value(<<"voice">>, Data)
              ,kz_json:find(<<"language">>, [Macro, Data])
              ,kz_json:find(<<"terminators">>, [Macro, Data], ?ANY_DIGIT)
              ],
    case tts_macro(TTSData) of
        'undefined' -> Acc;
        TTSMacro ->
            {[TTSMacro | Macros], Data, AccountId}
    end;
get_macro_entry(Macro, {Macros, Data, AccountId}, <<"tone">>) ->
    Tone = kz_json:delete_key(<<"macro">>, Macro),
    {[{'tones', [Tone]} | Macros], Data, AccountId}.

-spec say_macro(kz_term:api_ne_binaries()) ->
                       kapps_call_command:audio_macro_prompt() | 'undefined'.
say_macro(SayArgs) ->
    macro(SayArgs, 'say').

-spec tts_macro(kz_term:api_ne_binaries()) ->
                       kapps_call_command:audio_macro_prompt() | 'undefined'.
tts_macro(TTSArgs) ->
    macro(TTSArgs, 'tts').


-spec macro(kz_term:api_ne_binaries(), atom()) ->
                   kapps_call_command:audio_macro_prompt() | 'undefined'.
macro(Args, Type) ->
    case lists:takewhile(fun kz_term:is_not_empty/1, Args) of
        [] -> 'undefined';
        Macro -> list_to_tuple([Type | Macro])
    end.

-ifdef(TEST).

schema_test_() ->
    Data = <<"{\"macros\":[
               {\"macro\":\"play\"
                ,\"id\":\"play_me\"
               }
             ,{\"macro\":\"tts\"
                ,\"text\":\"this can be said\"
                ,\"language\":\"en-us\"
               }
             ,{\"macro\":\"prompt\"
                ,\"id\":\"vm-enter_pin\"
               }
             ,{\"macro\":\"say\"
                ,\"text\":\"123\"
                ,\"method\":\"pronounced\"
                ,\"type\":\"number\"
               }
             ,{\"macro\":\"tone\"
                ,\"frequencies\":[400,450]
                ,\"duration_on\":400
                ,\"duration_off\":200
               }
             ]
              }">>,

    build_tests(Data).

another_schema_test_() ->
    Data = <<"{
        \"macros\": [
            {
                \"macro\": \"play\",
                \"id\": \"http://server.com/you-pressed-1.wav\",
                \"endless_playback\": false
            },
            {
                \"macro\": \"play\",
                \"id\": \"http://server.com/you-pressed-2.wav\",
                \"endless_playback\": false
            },
            {
                \"macro\": \"play\",
                \"id\": \"http://server.com/something.mp3\",
                \"endless_playback\": false
            }
        ],
        \"terminators\": [
            \"1\",
            \"2\",
            \"3\",
            \"4\",
            \"5\",
            \"6\",
            \"7\",
            \"8\",
            \"9\",
            \"*\",
            \"0\",
            \"#\"
        ]
    }">>,
    build_tests(Data).

build_tests(Data) ->
    lists:foldl(fun(TestF, Tests) ->
                        TestF(Data) ++ Tests
                end
               ,[]
               ,[fun validate/1
                 ,fun build_macro_command/1
                ]
               ).

build_macro_command(DataBin) ->
    Data = kz_json:decode(DataBin),
    M = get_macro(Data, kapps_call:new()),
    [?_assert(is_list(M))].

validate(Data) ->
    Validated = kz_json_schema:validate(<<"callflows.audio_macro">>, kz_json:decode(Data)),
    'error' =:= element(1, Validated)
        andalso ?LOG_DEBUG("validated: ~p", [Validated]),
    [?_assertMatch({'ok', _}, Validated)].

-endif.
