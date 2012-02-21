%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 15 Aug 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(callflow_maintenance).

-export([blocking_refresh/0]).
-export([refresh/0, refresh/1]).
-export([migrate_menus/0, migrate_menus/1]).
-export([show_calls/0]).
-export([flush/0]).

-include("callflow.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec flush/0 :: () -> 'ok'.
flush() ->
    wh_cache:flush().

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec show_calls/0 :: () -> 'ok'.
show_calls() ->
    do_show_calls(cf_exe_sup:workers(), 0).

do_show_calls([], Total) ->
    io:format("Total: ~p~n", [Total]);
do_show_calls([Srv|Srvs], Total) ->
    case catch(cf_exe:get_call_info(Srv)) of
        {ok, Call} -> 
            io:format("CF_EXE(~p): ~p~n", [Srv, whapps_call:to_proplist(Call)]);
        _ -> ok
    end,
    do_show_calls(Srvs, Total + 1).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec blocking_refresh/0 :: () -> 'ok'.
blocking_refresh() ->
    lists:foreach(fun(AccountDb) ->
                          refresh(AccountDb)
                  end, whapps_util:get_all_accounts()),
    ok.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> 'started'.
-spec refresh/1 :: (binary() | string()) -> 'ok'.

refresh() ->
    spawn(fun() ->
                  lists:foreach(fun(AccountDb) ->
                                        refresh(AccountDb)
                                end, whapps_util:get_all_accounts())
          end),
    started.

refresh(<<Account/binary>>) ->
    AccountDb = wh_util:format_account_id(Account, encoded),
    Views = whapps_util:get_views_json(callflow, "views"),
    whapps_util:update_views(AccountDb, Views);
refresh(Account) ->
    refresh(wh_util:to_binary(Account)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function will migrate all the menus mailbox documents to
%% the latest version.
%% @end
%%--------------------------------------------------------------------
-spec migrate_menus/0:: () -> [done | error,...].
-spec migrate_menus/1 :: (Account) -> done | error when
      Account :: binary().

migrate_menus() ->
    [ migrate_menus(Account) || Account <- whapps_util:get_all_accounts(raw) ].

migrate_menus(Account) ->
    Db = wh_util:format_account_id(Account, encoded),
    log("migrating all menus in ~s", [Db]),
    case couch_mgr:get_results(Db, {<<"menus">>, <<"crossbar_listing">>}, [{<<"include_docs">>, true}]) of
        {ok, []} ->
            log("db ~s has no menus", [Db]),
            done;
        {ok, Menus} ->
            [do_menu_migration(Menu, Db) || Menu <- Menus];
        {error, _E} ->
            log("unable to get a list of menus: ~p", [_E]),
            error
    end.

do_menu_migration(Menu, Db) ->
    Doc = wh_json:get_value(<<"doc">>, Menu),
    MenuId = wh_json:get_value(<<"_id">>, Doc),
    VSN = wh_json:get_integer_value(<<"pvt_vsn">>, Doc, 1),
    case couch_mgr:fetch_attachment(Db, MenuId, <<"prompt.mp3">>) of
        {ok, _} when VSN =/= 1 ->
            log("menu ~s in ~s already migrated", [MenuId, Db]),
            ok;
        {ok, Bin} ->
            Name = <<(wh_json:get_value(<<"name">>, Doc, <<>>))/binary, " menu greeting">>,
            MediaId = create_media_doc(Name, <<"menu">>, MenuId, Db),
            AName = <<(wh_util:to_hex_binary(crypto:rand_bytes(16)))/binary, ".mp3">>,
            {ok, _} = couch_mgr:put_attachment(Db, MediaId, AName, Bin),
            ok = update_doc([<<"media">>, <<"greeting">>], MediaId, MenuId, Db),
            ok = update_doc([<<"pvt_vsn">>], <<"2">>, MenuId, Db),
            log("migrated menu ~s in ~s prompt to /~s/~s/~s", [MenuId, Db, Db, MediaId, AName]);
        _ ->
            log("menu ~s in ~s has no greeting or prompt", [MenuId, Db]),
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_media_doc/4 :: (binary(), binary(), binary(), binary()) -> binary().
create_media_doc(Name, SourceType, SourceId, Db) ->
    Props = [{<<"name">>, Name}
             ,{<<"description">>, <<SourceType/binary, " recorded/prompt media">>}
             ,{<<"source_type">>, SourceType}
             ,{<<"source_id">>, SourceId}
             ,{<<"content_type">>, <<"audio/mpeg">>}
             ,{<<"media_type">>, <<"mp3">>}
             ,{<<"streamable">>, true}],
    Doc = wh_doc:update_pvt_parameters(wh_json:from_list(Props), Db, [{type, <<"media">>}]),
    {ok, JObj} = couch_mgr:save_doc(Db, Doc),
    wh_json:get_value(<<"_id">>, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Simple log wrapper.
%% @end
%%--------------------------------------------------------------------
-spec log/2 :: (Format, Args) -> ok when
      Format :: string(),
      Args :: list().
log(Format, Args) ->
    io:format(Format ++ "~n", Args),
    ?LOG(Format, Args).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_doc/4 :: (list() | binary(), wh_json:json_term(), binary(), binary()) -> ok | {error, atom()}.
update_doc(Key, Value, Id, Db) ->
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} ->
            case couch_mgr:save_doc(Db, wh_json:set_value(Key, Value, JObj)) of
                {error, conflict} ->
                    update_doc(Key, Value, Id, Db);
                {ok, _} ->
                    ok;
                {error, _}=E ->
                    ?LOG("unable to update ~s in ~s, ~p", [Id, Db, E])
            end;
        {error, _}=E ->
            ?LOG("unable to update ~s in ~s, ~p", [Id, Db, E])
    end.
