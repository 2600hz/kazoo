%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_tasks).
%% behaviour: tasks_provider

-export([help/0
        ,category/0
        ,module/0
        ]).

%% Verifiers
-export([number/1
        ,account_id/1
        ,auth_by/1
        ,module_name/1
        ]).

%% Appliers
-export([%%list/2
        assign_to/4
        ,delete/3
        ,reserve/4
        ,add/5
        %,update_features/5
        ]).

-include("knm.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-spec category() -> ne_binary().
category() -> <<"number_management">>.

-spec module() -> module().
module() -> kz_util:to_binary(?MODULE).

-spec help() -> kz_proplist().
help() ->
    %% [{<<"list">>
    %%  ,kz_json:from_list([{<<"description">>, <<"List all numbers in the system">>}
    %%                     ,{<<"mandatory">>, [
    %%                                        ]}
    %%                     ,{<<"optional">>, [<<"auth_by">>
    %%                                       ]}
    %%                     ])
    %%  }

    [{<<"assign_to">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-assign numbers to the provided account">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"delete">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-remove numbers">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"reserve">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-move numbers to reserved (adding if missing)">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ]}
                        ])
     }

    ,{<<"add">>
     ,kz_json:from_list([{<<"description">>, <<"Bulk-create numbers">>}
                        ,{<<"expected_content">>, <<"text/csv">>}
                        ,{<<"mandatory">>, [<<"number">>
                                           ,<<"account_id">>
                                           ]}
                        ,{<<"optional">>, [<<"auth_by">>
                                          ,<<"module_name">>
                                          ]}
                        ])
     }

    %% ,{<<"update_features">>
    %%  ,kz_json:from_list([{<<"description">>, <<"Bulk-update features of numbers">>}
    %%                     ,{<<"expected_content">>, <<"text/csv">>}
    %%                     ,{<<"mandatory">>, [<<"number">>
    %%                                        ]}
    %%                     ,{<<"optional">>, [<<"cnam.inbound">>
    %%                                       ,<<"cnam.outbound">>
    %%                                       ,<<"e911.street_address">>
    %%                                            %%TODO: exhaustive list
    %%                                       ]}
    %%                     ])
    %%  }
    ].

%% Verifiers

-spec number(ne_binary()) -> boolean().
number(<<"+", _/binary>>) -> 'true';
number(_) -> 'false'.

-spec account_id(ne_binary()) -> boolean().
account_id(?MATCH_ACCOUNT_RAW(_)) -> 'true';
account_id(_) -> 'false'.

-spec auth_by(api_binary()) -> boolean().
auth_by('undefined') -> 'true';
auth_by(?MATCH_ACCOUNT_RAW(_)) -> 'true';
auth_by(_) -> 'false'.

-spec module_name(api_binary()) -> boolean().
module_name('undefined') -> 'true';
module_name(Thing) ->
    Modules = [<<"knm_bandwidth2">>
              ,<<"knm_bandwidth">>
              ,<<"knm_carriers">>
              ,<<"knm_inum">>
              ,<<"knm_local">>
              ,<<"knm_managed">>
              ,<<"knm_other">>
              ,<<"knm_simwood">>
              ,<<"knm_vitelity">>
              ,<<"knm_voip_innovations">>
              ],
    lists:member(Thing, Modules).

%% Appliers

%% -spec list(kz_proplist(), api_binary()) -> any().
%% list(Props, AuthBy0) ->
%%     AuthBy = case AuthBy0 of
%%                  'undefined' -> props:get_value('auth_account_id', Props);
%%                  _ -> AuthBy0
%%              end,
%%     Options =

-spec assign_to(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> task_return().
assign_to(Props, Number, AccountId, AuthBy) ->
    Options = [{'auth_by', get_auth_by(AuthBy, Props)}
              ],
    handle_result(knm_number:move(Number, AccountId, Options)).

-spec delete(kz_proplist(), ne_binary(), api_binary()) -> task_return().
delete(Props, Number, AuthBy) ->
    Options = [{'auth_by', get_auth_by(AuthBy, Props)}
              ],
    handle_result(knm_number:release(Number, Options)).

-spec reserve(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> task_return().
reserve(Props, Number, AccountId, AuthBy) ->
    Options = [{'auth_by', get_auth_by(AuthBy, Props)}
              ,{'assign_to', AccountId}
              ],
    handle_result(knm_number:reserve(Number, Options)).

-spec add(kz_proplist(), ne_binary(), ne_binary(), api_binary(), api_binary()) -> task_return().
add(Props, Number, AccountId, AuthBy, ModuleName0) ->
    Options = [{'auth_by', get_auth_by(AuthBy, Props)}
              ,{'assign_to', AccountId}
              ,{'module_name', ModuleName0}
              ],
    handle_result(knm_number:create(Number, Options)).

%% -spec update_features(kz_proplist(), ne_binary(), api_binary(), ...) -> any().
%% update_features(Props, Number, CNAMInbound0, ...) ->


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec handle_result(knm_number_return()) -> task_return().
handle_result({'ok', _KNMNumber}) -> 'ok';
handle_result({'dry_run', _Services, _Charges}) -> <<"accept_charges">>;
handle_result({'error', Reason})
  when is_atom(Reason) ->
    kz_util:to_binary(Reason);
handle_result({'error', KNMError}) ->
    case knm_errors:message(KNMError) of
        'undefined' -> knm_errors:error(KNMError);
        Reason -> Reason
    end.

-spec get_auth_by(api_binary()) -> api_binary().
get_auth_by('undefined', Props) -> props:get_value('auth_account_id', Props);
get_auth_by(AuthBy, _) -> AuthBy.

%% End of Module.
