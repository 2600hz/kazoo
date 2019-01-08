%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_response).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).

%%------------------------------------------------------------------------------
%% @doc Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Code0 = kz_json:get_integer_value(<<"code">>, Data, 486),
    Code = kz_term:to_binary(Code0),
    Cause = kz_json:get_ne_binary_value(<<"message">>, Data),
    Media = kz_media_util:media_path(kz_json:get_binary_value(<<"media">>, Data)
                                    ,kapps_call:account_id(Call)
                                    ),
    lager:info("responding to call with ~s ~s", [Code, Cause]),
    _ = kapps_call_command:response(Code, Cause, Media, Call),
    cf_exe:stop(Call).
