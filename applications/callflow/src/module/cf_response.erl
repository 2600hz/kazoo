%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_response).

-include("callflow.hrl").

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
    Media = wh_media_util:media_path(wh_json:get_value(<<"media">>, Data), Call),
    lager:info("responding to call with ~s ~s", [Code, Cause]),
    _ = whapps_call_command:response(Code, Cause, Media, Call),
    cf_exe:stop(Call).
