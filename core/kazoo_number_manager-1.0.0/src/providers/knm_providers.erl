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
-spec save(knm_phone_number:knm_number()) -> number_return().
save(PhoneNumber) ->
    exec(PhoneNumber, 'save').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec delete(knm_phone_number:knm_number()) -> number_return().
delete(PhoneNumber) ->
    exec(PhoneNumber, 'delete').

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-type exec_action() :: 'save' | 'delete'.
-spec exec(knm_phone_number:knm_number(), exec_action()) -> number_return().
-spec exec(knm_phone_number:knm_number(), exec_action(), ne_binaries()) -> number_return().
exec(PhoneNumber, Action) ->
    Providers = whapps_config:get(?KNM_CONFIG_CAT, <<"providers">>, ?DEFAULT_PROVIDER_MODULES),
    exec(PhoneNumber, Action, Providers).

exec(PhoneNumber, _, []) ->
    {'ok', PhoneNumber};
exec(PhoneNumber, Action, [Provider|Providers]) ->
    case wh_util:try_load_module(<<"knm_", Provider/binary>>) of
        'false' ->
            lager:debug("provider ~s is unknown, skipping", [Provider]),
            exec(PhoneNumber, Action, Providers);
        Module ->
            case erlang:apply(Module, Action, [PhoneNumber]) of
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
