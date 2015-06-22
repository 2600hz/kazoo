%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600hz INC
%%% @doc
%%% Handle prepend feature
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_provider).

-include("../knm.hrl").

-export([save/1]).
-export([delete/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(number()) -> number_return().
save(Number) ->
    exec(Number, 'save').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(number()) -> number_return().
delete(Number) ->
    exec(Number, 'delete').


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec exec(number(), atom()) -> number_return().
-spec exec(number(), atom(), ne_binaries()) -> number_return().
exec(Number, Action) ->
    Providers = whapps_config:get(?KNM_CONFIG_CAT, <<"providers">>, ?DEFAULT_PROVIDER_MODULES),
    exec(Number, Action, Providers).

exec(Number, _, []) -> {'ok', Number};
exec(Number, Action, [Provider|Providers]) ->
    case wh_util:try_load_module(<<"wnm_", Provider/binary>>) of
        'false' ->
            lager:debug("provider ~s is unknown, skipping", [Provider]),
            exec(Number, Action, Providers);
        Module ->
            case apply(Module, Action, [Number]) of
                {'ok', N}->
                    lager:debug("successfully attempted ~s:~s/1", [Module, Action]),
                    exec(N, Action, Providers);
                {'error', Reason} ->
                    lager:debug("failed attempting ~s:~s/1: ~p", [Module, Action, Reason]),
                    Error = wh_json:from_list([{Provider, Reason}]),
                    {'error', Error};
                {'invalid', Data} ->
                    lager:debug("failed attempting ~s:~s/1: ~p", [Module, Action, Data]),
                    Error = wh_json:set_value(<<"provider">>, Provider, Data),
                    {'error', Error};
                {'multiple_choice', Update} ->
                    lager:debug("update sent by ~s", [Module]),
                    Error = wh_json:from_list([{Provider, Update}]),
                    {'error', Error}
            end
    end.