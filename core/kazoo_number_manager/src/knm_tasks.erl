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

-export([number/1
        ]).

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

%% Appliers

%% -spec list(kz_proplist(), api_binary()) -> any().
%% list(Props, AuthBy0) ->
%%     AuthBy = case AuthBy0 of
%%                  'undefined' -> props:get_value('auth_account_id', Props);
%%                  _ -> AuthBy0
%%              end,
%%     Options =

-spec assign_to(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> any().
assign_to(Props, Number, AccountId, AuthBy0) ->
    AuthBy = case AuthBy0 of
                 'undefined' -> props:get_value('auth_account_id', Props);
                 _ -> AuthBy0
             end,
    Options = [{'auth_by', AuthBy}
              ],
    knm_number:move(Number, AccountId, Options).

-spec delete(kz_proplist(), ne_binary(), api_binary()) -> any().
delete(Props, Number, AuthBy0) ->
    AuthBy = case AuthBy0 of
                 'undefined' -> props:get_value('auth_account_id', Props);
                 _ -> AuthBy0
             end,
    Options = [{'auth_by', AuthBy}
              ],
    knm_number:release(Number, Options).

-spec reserve(kz_proplist(), ne_binary(), ne_binary(), api_binary()) -> any().
reserve(Props, Number, AccountId, AuthBy0) ->
    AuthBy = case AuthBy0 of
                 'undefined' -> props:get_value('auth_account_id', Props);
                 _ -> AuthBy0
             end,
    Options = [{'auth_by', AuthBy}
              ,{'assign_to', AccountId}
              ],
    knm_number:reserve(Number, Options).

-spec add(kz_proplist(), ne_binary(), ne_binary(), api_binary(), api_binary()) -> any().
add(Props, Number, AccountId, AuthBy0, ModuleName0) ->
    AuthBy = case AuthBy0 of
                 'undefined' -> props:get_value('auth_account_id', Props);
                 _ -> AuthBy0
             end,
    Options = [{'auth_by', AuthBy}
              ,{'assign_to', AccountId}
              ,{'module_name', ModuleName0}
              ],
    knm_number:create(Number, Options).

%% -spec update_features(kz_proplist(), ne_binary(), api_binary(), ...) -> any().
%% update_features(Props, Number, CNAMInbound0, ...) ->


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% End of Module.
