%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_config).

-export([record_min_sec/0
        ,storage_retry_times/1
        ,call_recording_extension/0
        ,should_store_recordings/0
        ]).

-include("kazoo_media.hrl").

-spec record_min_sec() -> non_neg_integer().
record_min_sec() ->
    kapps_config:get_integer(?CONFIG_CAT, <<"record_min_sec">>, 0).

-spec storage_retry_times(kz_term:ne_binary()) -> pos_integer().
storage_retry_times(AccountId) ->
    kapps_account_config:get_global(AccountId, ?CONFIG_CAT
                                   ,[<<"call_recording">>, <<"storage_retry_times">>]
                                   ,5
                                   ).

-spec call_recording_extension() -> kz_term:ne_binary().
call_recording_extension() ->
    kapps_config:get_ne_binary(?CONFIG_CAT, [<<"call_recording">>, <<"extension">>], <<"mp3">>).

-spec should_store_recordings() -> boolean().
should_store_recordings() ->
    kapps_config:get_is_true(?CONFIG_CAT, <<"store_recordings">>, 'false').
