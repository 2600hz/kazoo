%%%-------------------------------------------------------------------
%%% @copyright (C) 2014-2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kz_media_map).

-behaviour(gen_listener).

-export([start_link/0
         ,prompt_path/3
         ,handle_media_doc/2
         ,flush/0
        ]).

%% ETS related
-export([table_id/0
         ,table_options/0
         ,find_me_function/0
         ,gift_data/0
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-include("kazoo_media.hrl").
-include_lib("kazoo/include/kapi_conf.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'conf', [{'doc_type', <<"media">>}, 'federate']}]).

-define(RESPONDERS, [{{?MODULE, 'handle_media_doc'}
                     ,[{<<"configuration">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(media_map, {id :: ne_binary() %% account/prompt-id
                    ,account_id :: ne_binary()
                    ,prompt_id :: ne_binary()
                    ,languages = kz_json:new() :: kz_json:object() %% {"lang1":"path1", "lang2":"path2"}
                   }).
-type media_map() :: #media_map{}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    gen_listener:start_link({'local', ?SERVER}
                            ,?MODULE
                            ,[{'bindings', ?BINDINGS}
                              ,{'responders', ?RESPONDERS}
                              ,{'queue_name', ?QUEUE_NAME}       % optional to include
                              ,{'queue_options', ?QUEUE_OPTIONS} % optional to include
                              ,{'consume_options', ?CONSUME_OPTIONS} % optional to include
                             ]
                            ,[]
                           ).

-spec flush() -> 'ok'.
flush() ->
    gen_listener:cast(?MODULE, 'flush').

-spec prompt_path(ne_binary(), ne_binary(), ne_binary()) -> api_binary().
prompt_path(AccountId, PromptId, L) ->
    Language = kz_term:to_lower_binary(L),
    #media_map{languages=Langs} = get_map(AccountId, PromptId),
    case kz_json:get_first_defined(language_keys(Language), Langs) of
        'undefined' ->
            lager:debug("failed to find prompt ~s in ~p", [PromptId, language_keys(Language)]),
            case kz_services:find_reseller_id(AccountId) of
                AccountId -> default_prompt_path(PromptId, Language);
                ResellerId -> prompt_path(ResellerId, PromptId, L)
            end;
        Path -> Path
    end.

-spec default_prompt_path(ne_binary(), ne_binary()) -> api_binary().
default_prompt_path(PromptId, Language) ->
    #media_map{languages=Langs} = get_map(PromptId),
    lager:debug("checking default langs ~p", [default_language_keys(Language)]),
    kz_json:get_first_defined(default_language_keys(Language), Langs).

-spec handle_media_doc(kz_json:object(), kz_proplist()) -> 'ok'.
handle_media_doc(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),

    handle_media_doc_change(JObj, kz_json:get_value(<<"Event-Name">>, JObj)).

-spec handle_media_doc_change(kz_json:object(), ne_binary()) -> 'ok'.
handle_media_doc_change(JObj, ?DOC_DELETED) ->
    MediaId = kz_json:get_value(<<"ID">>, JObj),
    PromptId = extract_prompt_id(MediaId),
    Database = kz_json:get_value(<<"Database">>, JObj),
    gen_listener:cast(?SERVER, {'rm_mapping', Database, PromptId});
handle_media_doc_change(JObj, _Change) ->
    Db = kz_json:get_value(<<"Database">>, JObj),
    {'ok', Doc} = kz_datamgr:open_doc(Db, kz_json:get_value(<<"ID">>, JObj)),
    gen_listener:cast(?MODULE, {'add_mapping', Db, Doc}).

-spec extract_prompt_id(ne_binary()) -> ne_binary().
extract_prompt_id(<<_Lang:5/binary, "%2F", PromptId/binary>>) ->
    PromptId;
extract_prompt_id(<<_Lang:2/binary, "%2F", PromptId/binary>>) ->
    PromptId;
extract_prompt_id(MediaId) ->
    MediaId.

-spec table_id() -> ?MODULE.
table_id() -> ?MODULE.

-spec table_options() -> kz_proplist().
table_options() ->
    ['set'
     ,'protected'
     ,{'keypos', #media_map.id}
     ,'named_table'
    ].

-spec find_me_function() -> api_pid().
find_me_function() -> whereis(?SERVER).

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {'ok', #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({'add_mapping', ?KZ_MEDIA_DB, JObj}, _From, State) ->
    _ = maybe_add_prompt(?KZ_MEDIA_DB, JObj),
     {'reply', 'ok', State};
handle_call({'add_mapping', Account, JObj}, _From, State) ->
    _ = maybe_add_prompt(kz_accounts:format_account_id(Account), JObj),
    {'reply', 'ok', State};
handle_call({'rm_mapping', ?KZ_MEDIA_DB, PromptId}, _From, State) ->
    lager:debug("removing prompt mappings for ~s/~s", [?KZ_MEDIA_DB, PromptId]),
    {'reply', ets:delete(table_id(), mapping_id(?KZ_MEDIA_DB, PromptId)), State};
handle_call({'rm_mapping', Account, PromptId}, _From, State) ->
    AccountId = kz_accounts:format_account_id(Account),
    lager:debug("removing prompt mappings for ~s/~s", [AccountId, PromptId]),
    {'reply', ets:delete(table_id(), mapping_id(AccountId, PromptId)), State};
handle_call({'new_map', Map}, _From, State) ->
    {'reply', ets:insert_new(table_id(), Map), State};
handle_call({'insert_map', Map}, _From, State) ->
    {'reply', ets:insert(table_id(), Map), State};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
    {'reply', {'error', 'not_implemented'}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast('flush', State) ->
    ets:delete_all_objects(table_id()),
    lager:debug("flushed all media mappings"),
    _ = kz_util:spawn(fun init_map/0),
    {'noreply', State};
handle_cast({'add_mapping', ?KZ_MEDIA_DB, JObj}, State) ->
    _ = maybe_add_prompt(?KZ_MEDIA_DB, JObj),
    {'noreply', State};
handle_cast({'add_mapping', AccountId, JObj}, State) ->
    _ = maybe_add_prompt(kz_accounts:format_account_id(AccountId), JObj),
    {'noreply', State};
handle_cast({'rm_mapping', ?KZ_MEDIA_DB, PromptId}, State) ->
    lager:debug("removing prompt mappings for ~s/~s", [?KZ_MEDIA_DB, PromptId]),
    ets:delete(table_id(), mapping_id(?KZ_MEDIA_DB, PromptId)),
    {'noreply', State};
handle_cast({'rm_mapping', Account, PromptId}, State) ->
    AccountId = kz_accounts:format_account_id(Account),
    lager:debug("removing prompt mappings for ~s/~s", [AccountId, PromptId]),
    ets:delete(table_id(), mapping_id(AccountId, PromptId)),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({'ETS-TRANSFER', _TableId, _From, _GiftData}, State) ->
    lager:debug("recv control of ~p from ~p", [_TableId, _From]),
    _ = kz_util:spawn(fun init_map/0),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _State) ->
    {'reply', []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec init_map() -> 'ok'.
-spec init_map(ne_binary()) -> 'ok'.
-spec init_map(ne_binary(), ne_binary(), binary(), pos_integer(), fun()) -> 'ok'.
-spec init_map(ne_binary(), ne_binary(), binary(), pos_integer(), fun(), kz_json:objects()) -> 'ok'.
init_map() ->
    init_map(?KZ_MEDIA_DB).

init_map(Db) ->
    init_map(Db, <<"media/crossbar_listing">>, <<>>, 50, fun gen_listener:cast/2).

init_map(Db, View, StartKey, Limit, SendFun) ->
    Options = [{'startkey', StartKey}
               ,{'limit', Limit+1}
               ,'include_docs'
              ],
    case kz_datamgr:get_results(Db, View, Options) of
        {'ok', []} -> lager:debug("no more results in ~s:~s", [Db, View]);
        {'ok', ViewResults} -> init_map(Db, View, StartKey, Limit, SendFun, ViewResults);
        {'error', _E} -> lager:debug("error loading ~s in ~s: ~p", [View, Db, _E])
    end.

init_map(Db, View, _StartKey, Limit, SendFun, ViewResults) ->
    try lists:split(Limit, ViewResults) of
        {Results, []} ->
            add_mapping(Db, SendFun, Results),
            lager:debug("added the last of the view results from ~s", [View]);
        {Results, [NextJObj]} ->
            add_mapping(Db, SendFun, Results),
            Key = kz_json:get_value(<<"key">>, NextJObj),
            lager:debug("fetching more results, starting at ~s", [Key]),
            init_map(Db, View, Key, Limit, SendFun)
    catch
        'error':'badarg' ->
            add_mapping(Db, SendFun, ViewResults),
            lager:debug("added the last of the view results from ~s", [View])
    end.

-spec add_mapping(ne_binary(), fun(), kz_json:objects()) -> 'ok'.
-spec add_mapping(ne_binary(), fun(), kz_json:objects(), pid()) -> 'ok'.
add_mapping(Db, SendFun, JObjs) ->
    add_mapping(Db, SendFun, JObjs, whereis(?MODULE)).

add_mapping(Db, _SendFun, JObjs, Srv) when Srv =:= self() ->
    AccountId = kz_accounts:format_account_id(Db, 'raw'),
    _ = [maybe_add_prompt(AccountId, kz_json:get_value(<<"doc">>, JObj))
         || JObj <- JObjs
        ],
    'ok';
add_mapping(Db, SendFun, JObjs, Srv) ->
     AccountId = kz_accounts:format_account_id(Db, 'raw'),
    _ = [SendFun(Srv, {'add_mapping', AccountId, kz_json:get_value(<<"doc">>, JObj)}) || JObj <- JObjs],
     'ok'.

-spec maybe_add_prompt(ne_binary(), kz_json:object()) -> 'ok'.
-spec maybe_add_prompt(ne_binary(), kz_json:object(), api_binary()) -> 'ok'.
maybe_add_prompt(AccountId, JObj) ->
    maybe_add_prompt(AccountId
                    ,JObj
                    ,kz_json:get_first_defined([<<"prompt_id">>
                                               ,[<<"doc">>, <<"prompt_id">>]
                                               ]
                                              ,JObj
                                              )
                    ).

maybe_add_prompt(?KZ_MEDIA_DB, JObj, 'undefined') ->
    Id = kz_doc:id(JObj),
    MapId = mapping_id(?KZ_MEDIA_DB, Id),

    case ets:lookup(table_id(), MapId) of
        [] ->
            lager:warning("adding old system prompt ~s", [Id]),
            maybe_add_prompt(?KZ_MEDIA_DB, JObj, Id);
        [#media_map{languages=_Ls}] ->
            lager:debug("old prompt ~s being ignored, has languages ~p", [Id, _Ls])
    end;
maybe_add_prompt(_AccountId, _JObj, 'undefined') ->
    lager:debug("no prompt id, ignoring ~s for ~s", [kz_doc:id(_JObj), _AccountId]);
maybe_add_prompt(AccountId, JObj, PromptId) ->
    lager:debug("add prompt ~s to ~s (~s)", [PromptId, AccountId, kz_doc:id(JObj)]),
    Lang = kz_term:to_lower_binary(
             kz_json:get_value(<<"language">>, JObj, kz_media_util:prompt_language(AccountId))
            ),

    #media_map{languages=Langs}=Map = get_map(AccountId, PromptId),

    lager:debug("adding language ~s for prompt ~s to map for ~s", [Lang, PromptId, AccountId]),
    UpdatedMap = Map#media_map{account_id=AccountId
                              ,prompt_id=PromptId
                              ,languages=kz_json:set_value(Lang
                                                          ,kz_media_util:prompt_path(kz_doc:account_id(JObj, ?KZ_MEDIA_DB)
                                                                                    ,kz_doc:id(JObj)
                                                                                    )
                                                          ,Langs
                                                          )
                              },

    insert_map(UpdatedMap).

-spec insert_map(media_map()) -> 'ok' | 'true'.
-spec insert_map(media_map(), pid()) -> 'ok' | 'true'.
insert_map(Map) ->
    insert_map(Map, whereis(?MODULE)).
insert_map(Map, Srv) when Srv =:= self() ->
    ets:insert(table_id(), Map);
insert_map(Map, Srv) ->
    gen_listener:call(Srv, {'insert_map', Map}).

-spec get_map(ne_binary()) -> media_map().
-spec get_map(ne_binary(), ne_binary()) -> media_map().
get_map(PromptId) ->
    get_map(?KZ_MEDIA_DB, PromptId).

get_map(?KZ_MEDIA_DB = Db, PromptId) ->
    MapId = mapping_id(Db, PromptId),
    case ets:lookup(table_id(), MapId) of
        [Map] -> Map;
        [] ->
            #media_map{id=MapId
                       ,account_id=Db
                       ,prompt_id=PromptId
                       ,languages=kz_json:new()
                      }
    end;
get_map(AccountId, PromptId) ->
    MapId = mapping_id(AccountId, PromptId),
    case ets:lookup(table_id(), MapId) of
        [] ->
            lager:debug("failed to find map ~s for account, loading", [MapId]),
            _ = init_account_map(AccountId, PromptId),
            load_account_map(AccountId, PromptId);
        [Map] -> Map
    end.

-spec init_account_map(ne_binary(), ne_binary()) -> 'true'.
init_account_map(AccountId, PromptId) ->
    SystemMap = get_map(PromptId),
    MapId = mapping_id(AccountId, PromptId),

    AccountMap = SystemMap#media_map{id=MapId
                                    ,account_id=AccountId
                                    },
    new_map(AccountMap).

-spec new_map(media_map()) -> 'true'.
-spec new_map(media_map(), pid()) -> 'true'.
new_map(Map) ->
    new_map(Map, whereis(?MODULE)).

new_map(Map, Srv) when Srv =:= self() ->
    ets:insert_new(table_id(), Map);
new_map(Map, Srv) ->
   'true' = gen_listener:call(Srv, {'new_map', Map}).

-spec load_account_map(ne_binary(), ne_binary()) -> media_map().
load_account_map(AccountId, PromptId) ->
    lager:debug("attempting to load account map for ~s/~s", [AccountId, PromptId]),

    case kz_datamgr:get_results(kz_accounts:format_account_id(AccountId, 'encoded')
                               ,<<"media/listing_by_prompt">>
                               ,[{'startkey', [PromptId]}
                                 ,{'endkey', [PromptId, kz_json:new()]}
                                 ,{'reduce', 'false'}
                                 ,'include_docs'
                                ]
                              )
    of
        {'ok', []} ->
            lager:debug("account ~s has 0 languages for prompt ~s", [AccountId, PromptId]);
        {'ok', PromptFiles} ->
            lager:debug("account ~s has language prompts for prompt ~s", [AccountId, PromptId]),
            add_mapping(AccountId, fun gen_listener:call/2, PromptFiles);
        {'error', _E} ->
            lager:debug("failed to load account ~s prompts: ~p", [AccountId, _E])
    end,
    get_map(AccountId, PromptId).

-spec default_language_keys(ne_binary()) -> ne_binaries().
-spec language_keys(ne_binary()) -> ne_binaries().
-spec language_keys(ne_binary(), ne_binaries()) -> ne_binaries().
default_language_keys(Language) ->
    DefaultLanguage = kz_media_util:default_prompt_language(),
    language_keys(Language) ++ [DefaultLanguage].

language_keys(Language) ->
    language_keys(Language, []).

language_keys(<<Primary:2/binary>>, Acc) ->
    lists:reverse([Primary | Acc]);
language_keys(<<Primary:2/binary, "-", _Secondary:2/binary>> = Lang, Acc) ->
    language_keys(Primary, [Lang | Acc]);
language_keys(<<Primary:5/binary, "_", _Secondary:5/binary>> = Lang, Acc) ->
    language_keys(Primary, [Lang | Acc]);
language_keys(Lang, Acc) ->
    lists:reverse([Lang | Acc]).

-spec mapping_id(ne_binary(), ne_binary()) -> ne_binary().
mapping_id(AccountId, PromptId) ->
    list_to_binary([AccountId, "/", PromptId]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

language_keys_test() ->
    LangTests = [{<<"en">>, [<<"en">>]}
                 ,{<<"en-us">>, [<<"en-us">>, <<"en">>]}
                 ,{<<"en-us_fr-fr">>, [<<"en-us_fr-fr">>, <<"en-us">>, <<"en">>]}
                 ,{<<"foo-bar">>, [<<"foo-bar">>]}
                ],
    [?assertEqual(Result, language_keys(Lang)) || {Lang, Result} <- LangTests].

-endif.
