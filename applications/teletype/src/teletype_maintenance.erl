%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(teletype_maintenance).

-export([receipts/0
        ,restore_system_templates/0
        ,restore_system_template/1
        ]).
-export([remove_customization/1, remove_customization/2
        ,force_system_default/1, force_system_default/2
        ]).
-export([renderer_status/0]).
-export([start_module/1
        ,stop_module/1
        ,list_templates_from_db/1
        ]).
-export([register_views/0]).

-include("teletype.hrl").

-define(RECEIPT_FORMAT, " ~4.s | ~45.s | ~-45.s | ~-30.s | ~-30.s | ~-20.s~n").

-spec receipts() -> 'ok'.
receipts() ->
    io:format(?RECEIPT_FORMAT, [<<>>, <<"Call or Msg ID">>, <<"Receipt">>, <<"To">>, <<"From">>, <<"Time">>]),
    Receipts = kz_cache:filter_local(?CACHE_NAME, fun filter_receipts/2),
    Sorted = lists:usort(fun sort_receipts/2, Receipts),
    lists:foldl(fun print_receipt/2, 1, Sorted),
    'ok'.

-spec filter_receipts(any(), any()) -> boolean().
filter_receipts({'receipt', _R}, #email_receipt{}) -> 'true';
filter_receipts(_, _) -> 'false'.

-spec sort_receipts({_, email_receipt()}, {_, email_receipt()}) -> boolean().
sort_receipts({_, #email_receipt{timestamp=S1}}, {_, #email_receipt{timestamp=S2}}) ->
    S1 < S2.

-spec print_receipt({{'receipt', kz_term:ne_binary()}, email_receipt()}, pos_integer()) -> pos_integer().
print_receipt({{'receipt', Receipt}
              ,#email_receipt{to=To
                             ,from=From
                             ,timestamp=GregSecs
                             ,call_id=CallId
                             }}
             ,Count
             ) ->
    io:format(?RECEIPT_FORMAT, [kz_term:to_binary(Count)
                               ,CallId
                               ,receipt_for_printing(Receipt)
                               ,convert_for_printing(To)
                               ,convert_for_printing(From)
                               ,kz_time:pretty_print_datetime(GregSecs)
                               ]),
    Count+1.

-spec convert_for_printing(kz_term:ne_binary() | kz_term:ne_binaries()) -> kz_term:ne_binary().
convert_for_printing(<<_/binary>>=V) -> V;
convert_for_printing([_|_]=Vs) -> kz_binary:join(Vs, <<",">>).

-spec receipt_for_printing(kz_term:ne_binary()) -> kz_term:ne_binary().
receipt_for_printing(Receipt) ->
    case re:run(Receipt
               ,<<"^2.0.0 Ok: queued as ([[:alnum:]]+).*\$">>
               ,[{'capture', 'all_but_first', 'binary'}]
               )
    of
        {'match', [QueuedReceipt]} ->
            <<"Queued as ", QueuedReceipt/binary>>;
        _ -> default_receipt_printing(Receipt)
    end.

default_receipt_printing(Receipt) ->
    kz_binary:strip(Receipt, [$\n, $\r]).

-spec restore_system_templates() -> ok.
restore_system_templates() ->
    lists:foreach(fun restore_system_template/1, list_templates_from_db(?KZ_CONFIG_DB)).

-spec restore_system_template(kz_term:ne_binary()) -> ok.
restore_system_template(<<"skel">>) -> 'ok';
restore_system_template(TemplateId) ->
    DbId = kz_notification:db_id(TemplateId),
    ModId = kz_notification:resp_id(TemplateId),

    io:format("restoring to default version: ~s(~s)~n", [ModId, DbId]),

    {'ok', TemplateDoc} = kz_datamgr:open_doc(?KZ_CONFIG_DB, DbId),
    {'ok', _Deleted} = kz_datamgr:del_doc(?KZ_CONFIG_DB, TemplateDoc),
    io:format("  deleted ~s~n", [TemplateId]),

    Mod = kz_term:to_atom(<<"teletype_", ModId/binary>>, 'true'),
    io:format("  re-initializing template ~s~n", [ModId]),
    try Mod:init() of
        'ok' -> io:format("  finished~n")
    catch
        ?STACKTRACE(_E, _T, ST)
        io:format("  crashed for reason ~p:~p ~n", [_E, _T]),
        kz_log:log_stacktrace(ST),
        io:format("St: ~p~n~n", [ST])

        end.

-spec list_templates_from_db(kz_term:ne_binary()) -> kz_term:ne_binaries().
list_templates_from_db(Db) ->
    case kz_datamgr:all_docs(Db
                            ,[{'startkey', <<"notification.">>}
                             ,{'endkey', <<"notification.zzz">>}
                             ]
                            )
    of
        {'ok', Results} ->
            [Id
             || Result <- Results,
                Id <- [kz_doc:id(Result)],
                'notification.skel' =/=  Id
            ];
        {'error', _E} ->
            io:format("failed to query existing notifications: ~p~n", [_E]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc Remove Template Customization from an account
%% @end
%%------------------------------------------------------------------------------
-spec remove_customization(kz_term:ne_binary()) -> 'no_return'.
remove_customization(Account) ->
    remove_customization(Account, list_templates_from_db(kz_util:format_account_db(Account))).

-spec remove_customization(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()) -> 'no_return'.
remove_customization(Account, Id) when is_binary(Id) ->
    remove_customization(Account, [kz_notification:db_id(Id)]);
remove_customization(_Account, []) ->
    io:format(":: no template customization(s) found for ~s~n", [_Account]),
    'no_return';
remove_customization(Account, Ids) ->
    io:format(":: removing ~b template customization(s) from ~s~n", [length(Ids), Account]),
    case kz_datamgr:del_docs(kz_util:format_account_db(Account), Ids) of
        {'ok', JObjs} ->
            _ = [io:format("  ~s: ~s~n", [kz_notification:resp_id(kz_doc:id(J)), kz_json:get_value(<<"error">>, J, <<"deleted">>)])
                 || J <- JObjs
                ],
            'no_return';
        {'error', _Reason} ->
            io:format("failed to remove customization: ~p", [_Reason]),
            'no_return'
    end.

%%------------------------------------------------------------------------------
%% @doc Forcing System's Templates to an account by first removing
%% account's customization and then copy the templates from
%% system_config to account's db.
%% @end
%%------------------------------------------------------------------------------
-spec force_system_default(kz_term:ne_binary()) -> 'no_return'.
force_system_default(Account) ->
    force_system_default(Account, list_templates_from_db(?KZ_CONFIG_DB)).

-spec force_system_default(kz_term:ne_binary(), kz_term:ne_binary() | kz_term:ne_binaries()) -> 'no_return'.
force_system_default(Account, Id) when is_binary(Id) ->
    force_system_default(Account, [kz_notification:db_id(Id)]);
force_system_default(_Account, []) -> 'no_return';
force_system_default(Account, Ids) ->
    _ = remove_customization(Account),
    io:format("~n:: forcing ~b system default template(s) for account ~s~n", [length(Ids), Account]),
    AccountDb = kz_util:format_account_db(Account),
    _ = [copy_from_system_to_account(AccountDb, Id) || Id <- Ids],
    'no_return'.

-spec copy_from_system_to_account(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
copy_from_system_to_account(AccountDb, Id) ->
    case kz_datamgr:copy_doc(?KZ_CONFIG_DB, Id, AccountDb, Id, []) of
        {'ok', _} -> io:format("  ~s: done~n", [kz_notification:resp_id(Id)]);
        {'error', _Reason} -> io:format("  ~s: ~p~n", [kz_notification:resp_id(Id), _Reason])
    end.

-spec renderer_status() -> 'no_return'.
renderer_status() ->
    Workers = [{Pid, process_info(Pid, 'message_queue_len')}
               || {_, Pid, _, _} <- gen_server:call(teletype_farms_sup:render_farm_name(), 'get_all_workers')
              ],
    {StateName, TotalWorkers, TotalOverflow, TotalInUse} = poolboy:status(teletype_farms_sup:render_farm_name()),
    io:format("Renderer Pool~n", []),
    io:format("  State           : ~s~n", [StateName]),
    io:format("  Total Workers   : ~p~n", [TotalWorkers]),
    io:format("  Overflow Workers: ~p~n", [TotalOverflow]),
    io:format("  Busy Workers    : ~p~n", [TotalInUse]),
    io:format("Renderer Workers~n", []),
    _ = [io:format("  ~p has ~p pending jobs~n", [Pid, QueueLength])
         || {Pid, {_, QueueLength}} <- Workers
        ],
    'no_return'.

-spec start_module(module() | kz_term:ne_binary()) -> 'ok' | {'error', any()}.
start_module(Module) when is_atom(Module) ->
    lager:info("starting teletype module ~s", [Module]),
    try Module:init() of
        _ -> maybe_add_module_to_autoload(Module)
    catch
        ?STACKTRACE(_Type, Reason, ST)
        lager:error("failed to start teletype module ~s with reason: ~s ~p"
                   ,[Module, _Type, Reason]
                   ),
        kz_log:log_stacktrace(ST),
        {'error', Reason}
        end;
start_module(Module) ->
    case module_from_binary(Module) of
        {'ok', Atom} -> start_module(Atom);
        Err -> Err
    end.

-spec stop_module(module() | kz_term:ne_binary()) -> 'ok' | {'error', any()}.
stop_module(Module) when is_atom(Module) ->
    teletype_bindings:flush_mod(Module),
    maybe_remove_module_from_autoload(Module);
stop_module(Module) ->
    case module_from_binary(Module) of
        {'ok', Atom} -> stop_module(Atom);
        Err -> Err
    end.

-spec module_from_binary(kz_term:ne_binary()) ->
                                {'ok', module()} |
                                {'error', 'invalid_mod'}.
module_from_binary(<<"teletype_", _/binary>> = Template) ->
    case kz_module:ensure_loaded(Template) of
        'false' -> invalid_module(Template);
        Module -> {'ok', Module}
    end;
module_from_binary(?NE_BINARY=Module) ->
    module_from_binary(<<"teletype_", Module/binary>>).

-spec invalid_module(kz_term:ne_binary()) -> {'error', 'invalid_mod'}.
invalid_module(Module) ->
    lager:error("~s is not a valid teletype module", [Module]),
    {'error', 'invalid_mod'}.

-spec maybe_add_module_to_autoload(kz_term:ne_binary() | module()) -> 'ok'.
maybe_add_module_to_autoload(Module) when is_binary(Module) ->
    Autoload = ?AUTOLOAD_MODULES,
    case lists:member(Module, Autoload) of
        'true' -> 'ok';
        'false' ->
            {'ok', _} = kapps_config:set_default(?NOTIFY_CONFIG_CAT
                                                ,?AUTOLOAD_MODULES_KEY
                                                ,lists:usort([Module | Autoload])
                                                ),
            io:format("added module ~s to autoloaded teletype modules~n", [Module])
    end;
maybe_add_module_to_autoload(Module) ->
    maybe_add_module_to_autoload(kz_term:to_binary(Module)).

-spec maybe_remove_module_from_autoload(kz_term:ne_binary() | module()) -> 'ok'.
maybe_remove_module_from_autoload(Module) when is_binary(Module) ->
    Autoload = ?AUTOLOAD_MODULES,
    case lists:member(Module, Autoload) of
        'false' -> 'ok';
        'true' ->
            {'ok', _} = kapps_config:set_default(?NOTIFY_CONFIG_CAT
                                                ,?AUTOLOAD_MODULES_KEY
                                                ,lists:delete(Module, Autoload)
                                                ),
            io:format("removed module ~s to autoloaded teletype modules~n", [Module])
    end;
maybe_remove_module_from_autoload(Module) ->
    maybe_remove_module_from_autoload(kz_term:to_binary(Module)).

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder(?APP).
