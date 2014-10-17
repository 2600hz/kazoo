%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(notify_maintenance).

-include("notify.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-export([check_initial_call/1]).
-export([check_initial_registration/1]).
-export([refresh/0]).
-export([refresh_template/0]).

-define(TEMPLATE_PATH, code:lib_dir('notify', 'priv')).
-define(SYSTEM_CONFIG_DB, <<"system_config">>).

-type input_term() :: atom() | string() | ne_binary().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns wether an accounts initial call notification has been sent
%% @end
%%--------------------------------------------------------------------
-spec check_initial_call(input_term()) -> 'ok' | 'failed'.
check_initial_call(Account) when is_binary(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} ->
            case wh_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_call">>], JObj) of
                'true' -> 'yes';
                'false' -> 'no'
            end;
        {'error', _R} ->
            lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns wether the initial_registration notification has been sent
%% @end
%%--------------------------------------------------------------------
-spec check_initial_registration(input_term()) -> 'ok' | 'failed'.
check_initial_registration(Account) when is_binary(Account) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case couch_mgr:open_cache_doc(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', JObj} ->
            case wh_json:is_true([<<"notifications">>, <<"first_occurrence">>, <<"sent_initial_registration">>], JObj) of
                'true' -> 'yes';
                'false' -> 'no'
        end;
        {'error', _R} ->
            lager:warning("unable to open account definition for ~s: ~p", [AccountId, _R])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    couch_mgr:db_create(?WH_ACCOUNTS_DB),
    Views = [whapps_util:get_view_json('notify', <<"views/notify.json">>)],
    whapps_util:update_views(?WH_ACCOUNTS_DB, Views),
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
            case wh_util:to_binary(File) of
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
    {'ok', JObjs} =  couch_mgr:all_docs(?SYSTEM_CONFIG_DB),
    lists:foldl(
        fun(JObj, Acc) ->
           case wh_json:get_value(<<"id">>, JObj) of
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
-spec match_file_to_db(ne_binaries(), ne_binaries()) -> wh_proplist().
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
    module_exists(wh_util:to_atom(Module, 'true')).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec compare_template_system_config(wh_proplist()) -> 'ok'.
-spec compare_template_system_config(wh_proplist(), wh_json:object()) -> 'ok'.
-spec compare_template_system_config(api_binary(), api_binary(), api_binary()) -> 'ok'.

compare_template_system_config([]) -> 'ok';
compare_template_system_config([{FileId, Id}|Match]) ->
    File = [?TEMPLATE_PATH, "/" ,wh_util:to_list(FileId)],
    case {open_file(File), open_system_config(Id)} of
        {'error', _} ->
            io:format("comparing, failed to open template: ~s~n", [FileId]),
            compare_template_system_config(Match);
        {_, 'error'} ->
            io:format("comparing, failed to open document: ~s~n", [Id]),
            compare_template_system_config(Match);
        {Props, 'not_found'} ->
            io:format("comparing, failed to open document (does not exist): ~s~n", [Id]),
            JObj =
                wh_json:from_list([
                    {<<"_id">>, Id}
                    ,{<<"default">>, wh_json:new()}
                ]),
            compare_template_system_config(Props, JObj),
            compare_template_system_config(Match);
        {Props, JObj} ->
            io:format("comparing, found template & doc: ~s & ~s~n", [FileId, Id]),
            compare_template_system_config(Props, JObj),
            compare_template_system_config(Match)
    end.


compare_template_system_config([], JObj) ->
    Id = wh_json:get_value(<<"_id">>, JObj),
    case couch_mgr:save_doc(?SYSTEM_CONFIG_DB, JObj) of
        {'ok', _} -> io:format("doc ~s updated~n", [Id]);
        {'error', Reason} ->
            io:format("doc ~s failed to update: ~p~n", [Id, Reason])
    end;
compare_template_system_config([{Key, FileTemplate}|Props], JObj) ->
    BinKey = wh_util:to_binary(Key),
    <<"notify.", Id/binary>> = wh_json:get_value(<<"_id">>, JObj),
    DefaultTemplates = props:get_value(Key, props:get_value(Id, ?NOTIFY_TEMPLATES, [])),
    case wh_json:get_value([<<"default">>, BinKey], JObj) of
        'undefined' ->
            io:format("key: '~s' not found in jobj adding file template value~n", [BinKey]),
            compare_template_system_config(Props, set_template(BinKey, FileTemplate, JObj));
        DocTeamplate ->
            case compare_template_system_config(DefaultTemplates, DocTeamplate, FileTemplate) of
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
    io:format("default template is undefined~n", []);
compare_template_system_config(_, 'undefined', _) ->
    io:format("doc template is undefined~n", []);
compare_template_system_config(_, _, 'undefined') ->
    io:format("file template is undefined~n", []);
compare_template_system_config(DefaultTemplates, DocTeamplate, FileTemplate) ->
    case DocTeamplate =:= FileTemplate of
        'true' -> 'default';
        'false' ->
            case lists:member(DocTeamplate, DefaultTemplates) of
                'true' -> 'file';
                'false' -> 'doc'
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec set_template(ne_binary(), ne_binary(), wh_json:object()) -> wh_json:object().
set_template(Key, Template, JObj) ->
    Default = wh_json:get_value(<<"default">>, JObj),
    wh_json:set_value(
        <<"default">>
        ,wh_json:set_value(Key, Template, Default)
        ,JObj
    ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec open_file(list()) -> wh_proplist() | 'error'.
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
-spec open_system_config(ne_binary()) -> wh_json:object() | 'error' | 'not_found'.
open_system_config(Id) ->
    case couch_mgr:open_cache_doc(?SYSTEM_CONFIG_DB, Id) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} -> 'not_found';
        {'error', _R} -> 'error'
    end.



