%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600hz INC
%%% @doc
%%% Handle prepend feature
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(knm_providers).

-include("../knm.hrl").

-export([save/1]).
-export([delete/1]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec save(knm_number:knm_number()) ->
                  knm_number:knm_number().
save(Number) ->
    exec(Number, 'save').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_number:knm_number()) ->
                    knm_number:knm_number().
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
-type exec_action() :: 'save' | 'delete'.
-spec exec(knm_number:knm_number(), exec_action()) ->
                  knm_number:knm_number().
-spec exec(knm_number:knm_number(), exec_action(), ne_binaries()) ->
                  knm_number:knm_number().

-ifdef(TEST).
-define(PROVIDER_MODULES, ?DEFAULT_PROVIDER_MODULES).
-else.
-define(PROVIDER_MODULES
        ,whapps_config:get(?KNM_CONFIG_CAT
                           ,<<"providers">>
                           ,?DEFAULT_PROVIDER_MODULES
                          )
       ).
-endif.

exec(Number, Action) ->
    Providers = ?PROVIDER_MODULES,
    exec(Number, Action, Providers).

exec(Number, _, []) ->
    Number;
exec(Number, Action, [Provider|Providers]) ->
    case wh_util:try_load_module(<<"knm_", Provider/binary>>) of
        'false' ->
            lager:debug("provider ~s is unknown, skipping", [Provider]),
            exec(Number, Action, Providers);
        Module ->
            Number1 = erlang:apply(Module, Action, [Number]),
            lager:debug("successfully attempted ~s:~s/1", [Module, Action]),
            exec(Number1, Action, Providers)
    end.

    %%             {'error', Reason} ->
    %%                 lager:debug("failed attempting ~s:~s/1: ~p", [Module, Action, Reason]),
    %%                 Error = wh_json:from_list([{Provider, Reason}]),
    %%                 knm_errors:unspecified('failed_provider', Error);
    %%             {'invalid', Data} ->
    %%                 lager:debug("failed attempting ~s:~s/1: ~p", [Module, Action, Data]),
    %%                 Error = wh_json:set_value(<<"provider">>, Provider, Data),
    %%                 knm_errors:unspecified('invalid_data', Error);
    %%             {'multiple_choice', Update} ->
    %%                 lager:debug("update sent by ~s", [Module]),
    %%                 Error = wh_json:from_list([{Provider, Update}]),
    %%                 knm_errors:unspecified('multiple_choice', Error)
    %%         end
    %% end.
