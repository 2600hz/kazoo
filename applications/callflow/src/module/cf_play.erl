%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_play).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    AccountId = whapps_call:account_id(Call),
    case wh_json:get_value(<<"id">>, Data) of
        'undefined' -> lager:info("invalid data in the play callflow");
        <<"/system_media", _/binary>> = Path -> play(Data, Call, Path);
        <<"system_media", _/binary>> = Path -> play(Data, Call, Path);
        <<"local_stream://",_/binary>> = Path -> play(Data, Call, Path);
        <<"silence_stream://",_/binary>> = Path -> play(Data, Call, Path);
        Path when AccountId =/= 'undefined' ->
            lager:info("prepending media ID with /~s/", [AccountId]),
            Path1 = <<$/, (wh_util:to_binary(AccountId))/binary, $/, Path/binary>>,
            play(Data, Call, Path1);
        _Path ->
            lager:info("unable to play ~s, as account id is undefined", [_Path])
    end,
    cf_exe:continue(Call).

-spec play(wh_json:object(), whapps_call:call(), ne_binary()) -> whapps_api_std_return().
play(Data, Call, Media) ->
    case wh_json:is_false(<<"answer">>, Data) of
        'true' -> 'ok';
        'false' -> whapps_call_command:answer(Call)
    end,
    lager:info("playing media ~s", [Media]),
    _ = whapps_call_command:b_play(Media, Call).
