%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Voxter Communications
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Daniel Finke
%%%-------------------------------------------------------------------
-module(acdc_announcements).

%% API
-export([start_link/3]).
-export([init/3]).

-include("acdc.hrl").

-define(DEFAULT_ANNOUNCEMENTS_MEDIA, [{<<"you_are_at_position">>, <<"queue-you_are_at_position">>}
                                     ,{<<"in_the_queue">>, <<"queue-in_the_queue">>}
                                     ,{<<"increase_in_call_volume">>, <<"queue-increase_in_call_volume">>}
                                     ,{<<"the_estimated_wait_time_is">>, <<"queue-the_estimated_wait_time_is">>}
                                     ]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the announcements process
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(), kapps_call:call(), kz_proplist()) -> startlink_ret().
start_link(Manager, Call, Props) ->
    {'ok', kz_util:spawn_link(fun ?MODULE:init/3, [Manager, Call, Props])}.

%%--------------------------------------------------------------------
%% @doc
%% Initializes the announcements process
%%
%% @end
%%--------------------------------------------------------------------
-spec init(pid(), kapps_call:call(), kz_proplist()) -> 'no_return'.
init(Manager, Call, Props) ->
    Config = get_config(Props),
    State = init_state(Manager, Call, Config),
    loop(State).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Load config from props into map
%%
%% @end
%%--------------------------------------------------------------------
-spec get_config(kz_proplist()) -> map().
get_config(Props) ->
    #{position_announcements_enabled => props:get_is_true(<<"position_announcements_enabled">>, Props, 'false')
     ,wait_time_announcements_enabled => props:get_is_true(<<"wait_time_announcements_enabled">>, Props, 'false')
     ,announcements_interval => props:get_integer_value(<<"interval">>, Props, 30)
     ,announcements_media => announcements_media(Props)
     }.

%%--------------------------------------------------------------------
%% @doc
%% Get media file configuration from props
%%
%% @end
%%--------------------------------------------------------------------
-spec announcements_media(kz_proplist()) -> kz_proplist().
announcements_media(Props) ->
    case props:get_value(<<"media">>, Props) of
        'undefined' ->
            ?DEFAULT_ANNOUNCEMENTS_MEDIA;
        AnnouncementsMedia ->
            AnnouncementsMedia
    end.

%%--------------------------------------------------------------------
%% @doc
%% Initialize state for the announcements process
%%
%% @end
%%--------------------------------------------------------------------
-spec init_state(pid(), kapps_call:call(), map()) -> map().
init_state(Manager, Call, Config) ->
    #{manager => Manager
     ,call => Call
     ,config => Config
     ,last_average_wait_time => 'undefined'
     }.

%%--------------------------------------------------------------------
%% @doc
%% Loop entry point
%%
%% @end
%%--------------------------------------------------------------------
-spec loop(map()) -> 'no_return'.
loop(State) ->
    maybe_announce_position(State).

%%--------------------------------------------------------------------
%% @doc
%% Conditionally add position announcements prompts to playlist
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_announce_position(map()) -> 'no_return'.
maybe_announce_position(#{config := #{position_announcements_enabled := 'false'}}=State) ->
    maybe_announce_wait_time([], State);
maybe_announce_position(#{manager := Manager
                         ,call := Call
                         ,config := Config
                         }=State) ->
    Language = kapps_call:language(Call),
    Position = gen_listener:call(Manager, {'queue_position', kapps_call:call_id(Call)}),

    Prompts = [{'prompt', announcements_media_file(<<"you_are_at_position">>, Config), Language, <<"A">>}
              ,{'say', kz_term:to_binary(Position), <<"number">>}
              ,{'prompt', announcements_media_file(<<"in_the_queue">>, Config), Language, <<"A">>}],
    maybe_announce_wait_time(Prompts, State).

%%--------------------------------------------------------------------
%% @doc
%% Conditionally add wait time announcements prompts to playlist
%%
%% @end
%%--------------------------------------------------------------------
-spec maybe_announce_wait_time(kapps_call_command:audio_macro_prompts(), map()) -> 'no_return'.
maybe_announce_wait_time(PromptAcc, #{config := #{wait_time_announcements_enabled := 'false'}}=State) ->
    play_announcements(PromptAcc, State);
maybe_announce_wait_time(PromptAcc, #{call := Call
                                     ,config := Config
                                     ,last_average_wait_time := LastAverageWaitTime
                                     }=State) ->
    Language = kapps_call:language(Call),
    AverageWaitTime = get_average_wait_time(Call),

    PromptAcc1 = case LastAverageWaitTime =/= 'undefined'
                     andalso AverageWaitTime > LastAverageWaitTime
                 of
                     'true' ->
                         PromptAcc ++ [{'prompt', announcements_media_file(<<"increase_in_call_volume">>, Config), Language, <<"A">>}];
                     'false' ->
                         PromptAcc
                 end,
    PromptAcc2 = PromptAcc1 ++
        [{'prompt', announcements_media_file(<<"the_estimated_wait_time_is">>, Config), Language, <<"A">>}
        ,time_prompt(AverageWaitTime, Language)
        ],
    play_announcements(PromptAcc2, State#{last_average_wait_time := AverageWaitTime}).

%%--------------------------------------------------------------------
%% @doc
%% Play the prompts on the playlist, sleep till the next execution,
%% repeat
%%
%% @end
%%--------------------------------------------------------------------
-spec play_announcements(kapps_call_command:audio_macro_prompts(), map()) -> 'no_return'.
play_announcements(Prompts, #{call := Call
                             ,config := Config
                             }=State) ->
    kapps_call_command:audio_macro(Prompts, Call),

    AnnouncementsInterval = announcements_interval(Config),
    timer:sleep(AnnouncementsInterval * ?MILLISECONDS_IN_SECOND),
    loop(State).

%%--------------------------------------------------------------------
%% @doc
%% Get the average wait time from stats via AMQP
%%
%% @end
%%--------------------------------------------------------------------
-spec get_average_wait_time(kapps_call:call()) -> api_pos_integer().
get_average_wait_time(Call) ->
    AccountId = kapps_call:account_id(Call),
    QueueId = kapps_call:custom_channel_var(<<"Queue-ID">>, Call),
    Req = props:filter_undefined(
            [{<<"Account-ID">>, kapps_call:account_id(Call)}
            ,{<<"Queue-ID">>, QueueId}
             | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
            ]),
    case kapps_util:amqp_pool_request(Req
                                     ,fun kapi_acdc_stats:publish_current_calls_req/1
                                     ,fun kapi_acdc_stats:current_calls_resp_v/1
                                     )
    of
        {'error', E} ->
            lager:error("failed to receive current calls from AMQP: ~p", [E]),
            'undefined';
        {'ok', Resp} ->
            calculate_average_wait_time(Resp)
    end.

%%--------------------------------------------------------------------
%% @doc
%% Compute the average wait time based on the current_calls_resp
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_average_wait_time(kz_json:object()) -> pos_integer().
calculate_average_wait_time(JObj) ->
    WaitTimeCalls = kz_json:get_list_value(<<"Waiting">>, JObj, []) ++
        kz_json:get_list_value(<<"Handled">>, JObj, []) ++
        kz_json:get_list_value(<<"Processed">>, JObj, []),
    Abandoned = length(kz_json:get_list_value(<<"Abandoned">>, JObj, [])),
    Total = length(kz_json:get_list_value(<<"Abandoned">>, JObj, [])) +
        length(kz_json:get_list_value(<<"Handled">>, JObj, [])) +
        length(kz_json:get_list_value(<<"Processed">>, JObj, [])),

    TotalWait = lists:foldl(fun(Stat, Acc) ->
                                    Acc + kz_json:get_integer_value(<<"wait_time">>, Stat, 0)
                            end, 0, WaitTimeCalls),
    calculate_average_wait_time(Abandoned, Total, TotalWait).

%%--------------------------------------------------------------------
%% @doc
%% Calculate average wait time, considering div by 0
%%
%% @end
%%--------------------------------------------------------------------
-spec calculate_average_wait_time(pos_integer(), pos_integer(), pos_integer()) -> pos_integer().
calculate_average_wait_time(Total, Total, TotalWait) ->
    TotalWait;
calculate_average_wait_time(Abandoned, Total, TotalWait) ->
    TotalWait div (Total - Abandoned).

%%--------------------------------------------------------------------
%% @doc
%% Structure for time prompt entries
%%
%% @end
%%--------------------------------------------------------------------
-spec time_prompt(pos_integer(), binary()) -> {'prompt', ne_binary(), binary(), ne_binary()}.
time_prompt(Time, Language) ->
    {'prompt', time_prompt2(Time), Language, <<"A">>}.

%%--------------------------------------------------------------------
%% @doc
%% Returns the appropriate prompt name for the given average wait time
%%
%% @end
%%--------------------------------------------------------------------
-spec time_prompt2(pos_integer()) -> ne_binary().
time_prompt2(Time) when Time < ?SECONDS_IN_MINUTE ->
    <<"queue-less_than_1_minute">>;
time_prompt2(Time) when Time =< 5 * ?SECONDS_IN_MINUTE ->
    <<"queue-about_5_minutes">>;
time_prompt2(Time) when Time =< 10 * ?SECONDS_IN_MINUTE ->
    <<"queue-about_10_minutes">>;
time_prompt2(Time) when Time =< 15 * ?SECONDS_IN_MINUTE ->
    <<"queue-about_15_minutes">>;
time_prompt2(Time) when Time =< 30 * ?SECONDS_IN_MINUTE ->
    <<"queue-about_30_minutes">>;
time_prompt2(Time) when Time =< 45 * ?SECONDS_IN_MINUTE ->
    <<"queue-about_45_minutes">>;
time_prompt2(Time) when Time =< 60 * ?SECONDS_IN_MINUTE ->
    <<"queue-about_1_hour">>;
time_prompt2(_) ->
    <<"queue-at_least_1_hour">>.

%%--------------------------------------------------------------------
%% @doc
%% Return the time interval between announcements
%%
%% @end
%%--------------------------------------------------------------------
-spec announcements_interval(map()) -> non_neg_integer().
announcements_interval(#{announcements_interval := Interval}) ->
    Interval.

%%--------------------------------------------------------------------
%% @doc
%% Return the media file of a given name from the config
%%
%% @end
%%--------------------------------------------------------------------
-spec announcements_media_file(ne_binary(), map()) -> api_ne_binary().
announcements_media_file(Name, #{announcements_media := Media}) ->
    props:get_binary_value(Name, Media).
