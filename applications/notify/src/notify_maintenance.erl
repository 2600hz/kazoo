%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   Michael Dunton
%%%-------------------------------------------------------------------
-module(notify_maintenance).

-include("notify.hrl").

-export([check_initial_call/1]).
-export([check_initial_registration/1]).
-export([refresh/0]).
-export([refresh_template/0]).
-export([reload_smtp_configs/0]).
-export([configure_smtp_relay/1
        ,configure_smtp_username/1
        ,configure_smtp_password/1
        ,configure_smtp_auth/1
        ,configure_smtp_port/1
        ]).

-define(TEMPLATE_PATH, code:lib_dir('notify', 'priv')).
-define(SYSTEM_CONFIG_DB, <<"system_config">>).
-define(SMTP_CLIENT_DOC, <<"smtp_client">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns whether an account's initial call notification has been sent
%% @end
%%--------------------------------------------------------------------
-spec check_initial_call(ne_binary()) -> 'ok'.
check_initial_call(Account) when is_binary(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kz_datamgr:open_cache_doc(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} ->
            case kz_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>], JObj) of
                'true' -> io:format("account ~s has made their first call!~n", [AccountId]);
                'false' -> io:format("account ~s has yet to make their first call~n", [AccountId])
            end;
        {'error', _R} ->
            io:format("unable to open account doc ~s: ~p", [AccountId, _R])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns wether the initial_registration notification has been sent
%% @end
%%--------------------------------------------------------------------
-spec check_initial_registration(ne_binary()) -> 'ok'.
check_initial_registration(Account) when is_binary(Account) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kz_datamgr:open_cache_doc(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} ->
            case kz_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>], JObj) of
                'true' -> io:format("account ~s has registered successfully~n", [AccountId]);
                'false' -> io:format("account ~s has yet to register successfully~n", [AccountId])
            end;
        {'error', _R} ->
            io:format("unable to open account doc ~s: ~p", [AccountId, _R])
    end.
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Configures the Relay key of the SMTP_Client System Config document.
%% @end
%%---------------------------------------------------------------------
-spec configure_smtp_relay(ne_binary()) -> 'ok' | 'failed'.
configure_smtp_relay(Value) ->
    case update_smtp_client_document(<<"relay">>, Value) of
        {'ok', _} ->
            'ok';
        _Error ->
            'failed'
    end.
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Configures the username key of the SMTP_Client System Config document.
%% @end
%%--------------------------------------------------------------------
-spec configure_smtp_username(ne_binary()) -> 'ok' | 'failed'.
configure_smtp_username(Value) ->
    case update_smtp_client_document(<<"username">>, Value) of
        {'ok', _} ->
            'ok';
        _Error ->
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Configures the password key of the SMTP_Client System Config document.
%% @end
%%--------------------------------------------------------------------
-spec configure_smtp_password(ne_binary()) -> 'ok' | 'failed'.
configure_smtp_password(Value) ->
    case update_smtp_client_document(<<"password">>, Value) of
        {'ok', _} ->
            'ok';
        _Error ->
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Configures the username key of the SMTP_Client System Config document.
%% @end
%%--------------------------------------------------------------------
-spec configure_smtp_auth(ne_binary()) -> 'ok' | 'failed'.
configure_smtp_auth(Value) ->
    case update_smtp_client_document(<<"auth">>, Value) of
        {'ok', _} ->
            'ok';
        _Error ->
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Configures the port key of the SMTP_Client System Config document.
%% @end
%%--------------------------------------------------------------------
-spec configure_smtp_port(ne_binary()) -> 'ok' | 'failed'.
configure_smtp_port(Value) ->
    case update_smtp_client_document(<<"port">>, Value) of
        {'ok', _} ->
            'ok';
        _Error ->
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    kz_datamgr:db_create(?KZ_ACCOUNTS_DB),
    Views = [kapps_util:get_view_json('notify', <<"views/notify.json">>)],
    kapps_util:update_views(?KZ_ACCOUNTS_DB, Views),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec refresh_template() -> 'ok'.
refresh_template() ->
    io:format("searching for docs in: ~p ~n", [?SYSTEM_CONFIG_DB]),
    Ids = template_ids(),
    io:format("searching for files in: ~p ~n", [?TEMPLATE_PATH]),
    Files = template_files(),
    io:format("matching now~n", []),
    Match = match_file_to_db(Files, Ids),
    io:format("comparing now~n", []),
    compare_template_system_config(Match).


-spec reload_smtp_configs() -> 'ok'.
reload_smtp_configs() ->
    kapps_config:flush(<<"smtp_client">>),
    ok.
%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec template_files() -> ne_binaries().
template_files() ->
    {'ok', Files} = file:list_dir(?TEMPLATE_PATH),
    lists:foldl(
      fun(File, Acc) ->
              case kz_util:to_binary(File) of
                  <<"notify_", _/binary>>=Bin ->
                      [Bin|Acc];
                  _ -> Acc
              end
      end
               ,[]
               ,Files
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec template_ids() -> ne_binaries().
template_ids() ->
    {'ok', JObjs} =  kz_datamgr:all_docs(?SYSTEM_CONFIG_DB),
    lists:foldl(
      fun(JObj, Acc) ->
              case kz_doc:id(JObj) of
                  <<"notify.", _/binary>>=Id ->
                      [Id|Acc];
                  _ ->
                      Acc
              end
      end
               ,[]
               ,JObjs
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec match_file_to_db(ne_binaries(), ne_binaries()) -> kz_proplist().
match_file_to_db(Files, Ids) ->
    lists:foldl(
      fun(<<"notify_", FileKey/binary>>=File, Acc) ->
              Key = binary:replace(FileKey, <<".config">>, <<>>),
              Id = <<"notify.", Key/binary>>,
              case
                  {lists:member(Id, Ids)
                  ,module_exists(<<"notify_", Key/binary>>)
                  }
              of
                  {'true', 'true'} ->
                      io:format("found doc & module for template file: '~s' ~n", [File]),
                      [{File, Id}|Acc];
                  {'false', 'false'} ->
                      io:format("did not found doc & module for template file: '~s', ignoring... ~n", [File]),
                      Acc;
                  {'false', _} ->
                      io:format("did not found doc but module exist for template file: '~s', adding... ~n", [File]),
                      [{File, Id}|Acc];
                  {_, 'false'} ->
                      io:format("did not module for template file: '~s', ignoring... ~n", [File]),
                      Acc
              end
      end
               ,[]
               ,Files
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec module_exists(binary() | atom()) -> boolean().
module_exists(Module) when is_atom(Module) ->
    try Module:module_info() of
        _ -> 'true'
    catch
        _:_ -> 'false'
    end;
module_exists(Module) ->
    module_exists(kz_util:to_atom(Module, 'true')).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec compare_template_system_config(kz_proplist()) -> 'ok'.
-spec compare_template_system_config(kz_proplist(), kz_json:object()) -> 'ok'.
-spec compare_template_system_config(api_binaries(), api_binary(), api_binary()) ->
                                            'default' | 'file' |
                                            'doc' | 'ok'.

compare_template_system_config([]) -> 'ok';
compare_template_system_config([{FileId, Id}|Match]) ->
    File = [?TEMPLATE_PATH, "/" ,kz_util:to_list(FileId)],
    case {open_file(File), open_system_config(Id)} of
        {'error', _} ->
            io:format("comparing, failed to open template: ~s~n", [FileId]),
            compare_template_system_config(Match);
        {_, 'error'} ->
            io:format("comparing, failed to open document: ~s~n", [Id]),
            compare_template_system_config(Match);
        {Props, 'not_found'} ->
            io:format("comparing, failed to open document (does not exist): ~s~n", [Id]),
            JObj = kz_json:from_list([{<<"_id">>, Id}
                                     ,{<<"default">>, kz_json:new()}
                                     ]),
            compare_template_system_config(Props, JObj),
            compare_template_system_config(Match);
        {Props, JObj} ->
            io:format("comparing, found template & doc: ~s & ~s~n", [FileId, Id]),
            compare_template_system_config(Props, JObj),
            compare_template_system_config(Match)
    end.

compare_template_system_config([], JObj) ->
    Id = kz_doc:id(JObj),
    case kz_datamgr:save_doc(?SYSTEM_CONFIG_DB, JObj) of
        {'ok', _} -> io:format("doc ~s updated~n", [Id]);
        {'error', Reason} ->
            io:format("doc ~s failed to update: ~p~n", [Id, Reason])
    end;
compare_template_system_config([{Key, FileTemplate}|Props], JObj) ->
    BinKey = kz_util:to_binary(Key),
    <<"notify.", Id/binary>> = kz_doc:id(JObj),
    DefaultTemplates = props:get_value(Key, props:get_value(Id, ?NOTIFY_TEMPLATES, [])),
    case kz_json:get_value([<<"default">>, BinKey], JObj) of
        'undefined' ->
            io:format("key: '~s' not found in jobj adding file template value~n", [BinKey]),
            compare_template_system_config(Props, set_template(BinKey, FileTemplate, JObj));
        DocTemplate ->
            case compare_template_system_config(DefaultTemplates, DocTemplate, FileTemplate) of
                'ok' ->
                    io:format("key: '~s' one of the templates is undefined, ignoring...~n", [BinKey]),
                    compare_template_system_config(Props, JObj);
                'default' ->
                    io:format("key: '~s' file & doc value are equal, ignoring...~n", [BinKey]),
                    compare_template_system_config(Props, JObj);
                'file' ->
                    io:format("key: '~s' template file updated, updating...~n", [BinKey]),
                    compare_template_system_config(Props, set_template(BinKey, FileTemplate, JObj));
                'doc' ->
                    io:format("key: '~s' doc manually updated, ignoring...~n", [BinKey]),
                    compare_template_system_config(Props, JObj)
            end
    end.

compare_template_system_config('undefined', _, _) ->
    io:format("default template is undefined~n");
compare_template_system_config(_, 'undefined', _) ->
    io:format("doc template is undefined~n");
compare_template_system_config(_, _, 'undefined') ->
    io:format("file template is undefined~n");
compare_template_system_config(DefaultTemplates, DocTemplate, FileTemplate) ->
    case DocTemplate =:= FileTemplate of
        'true' -> 'default';
        'false' ->
            case lists:member(DocTemplate, DefaultTemplates) of
                'true' -> 'file';
                'false' -> 'doc'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_template(ne_binary(), ne_binary(), kz_json:object()) -> kz_json:object().
set_template(Key, Template, JObj) ->
    Default = kz_json:get_value(<<"default">>, JObj),
    kz_json:set_value(
      <<"default">>
                     ,kz_json:set_value(Key, Template, Default)
                     ,JObj
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec open_file(list()) -> kz_proplist() | 'error'.
open_file(File) ->
    case file:consult(File) of
        {'ok', Props} -> Props;
        {'error', _R} -> 'error'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec open_system_config(ne_binary()) -> kz_json:object() | 'error' | 'not_found'.
open_system_config(Id) ->
    case kz_datamgr:open_cache_doc(?SYSTEM_CONFIG_DB, Id) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} -> 'not_found';
        {'error', _R} -> 'error'
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec update_smtp_client_document(ne_binary(), ne_binary()) -> {'ok', kz_json:object()} | {error, any()}.
update_smtp_client_document(Key, Value) ->
    kapps_config:set(?SMTP_CLIENT_DOC, Key, Value).
