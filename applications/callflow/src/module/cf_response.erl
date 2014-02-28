%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_response).

-include("../callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Code = wh_json:get_binary_value(<<"code">>, Data, <<"486">>),
    Cause = wh_json:get_ne_value(<<"message">>, Data),
    Media = get_media_id(Data, Call),
    lager:info("responding to call with ~s ~s", [Code, Cause]),
    _ = whapps_call_command:response(Code, Cause, Media, Call),
    cf_exe:stop(Call).

-spec get_media_id(api_binary() | wh_json:object(), whapps_call:call()) -> api_binary().
get_media_id('undefined', _) -> 'undefined';
get_media_id(<<"system_media", _/binary>> = Path, _) -> Path;
get_media_id(<<"local_stream://", _/binary>> = Path, _) -> Path;
get_media_id(<<"silence_stream://", _/binary>> = Path, _) -> Path;
get_media_id(<<"/", _/binary>> = Path, _) -> Path;
get_media_id(Path, Call) when is_binary(Path) ->
    case whapps_call:account_id(Call) of
        'undefined' -> Path;
        AccountId ->
            lager:info("prepending media ID with /~s/", [AccountId]),
            <<$/, (wh_util:to_binary(AccountId))/binary, $/, Path/binary>>
    end;
get_media_id(Data, Call) ->
    get_media_id(wh_json:get_ne_value(<<"media">>, Data), Call).
