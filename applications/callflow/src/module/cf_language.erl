%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%% "data":{"language":"en"}
%%%
%%% Sets the language to use for the rest of the call
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(cf_language).

-include("callflow.hrl").

-export([handle/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> 'ok'.
handle(Data, Call) ->
    Lang = wh_json:get_value(<<"language">>, Data),
    lager:info("setting call's language to '~s'", [Lang]),
    Call1 = whapps_call:set_language(Lang, Call),

    whapps_call_command:set(
      wh_json:from_list([{<<"default_language">>, Lang}])
      ,'undefined'
      ,Call1
     ),

    cf_exe:set_call(Call1),

    cf_exe:continue(Call1).
