%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2014-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

%% By convention, we put the options here in macros, but not required.
-define(BINDINGS, [{'conf', [{'doc_type', <<"media">>}, 'federate']}]).

-define(RESPONDERS, [{{?MODULE, 'handle_media_doc'}
                     ,[{<<"configuration">>, <<"*">>}]
                     }
                    ]).
-define(QUEUE_NAME, <<>>).
-define(QUEUE_OPTIONS, []).
-define(CONSUME_OPTIONS, []).

-record(media_map, {id :: kz_term:ne_binary() %% account/prompt-id
                   ,account_id :: kz_term:ne_binary()
                   ,prompt_id :: kz_term:ne_binary()
                   ,languages = kz_json:new() :: kz_json:object() %% {"lang1":"path1", "lang2":"path2"}
                   }).
-type media_map() :: #media_map{}.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
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

-spec prompt_path(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
prompt_path(AccountId, PromptId, L) ->
    Language = kz_term:to_lower_binary(L),
    #media_map{languages=Langs} = get_map(AccountId, PromptId),
    case kz_json:get_first_defined(language_keys(Language), Langs) of
        'undefined' when ?KZ_MEDIA_DB =:= AccountId ->
            lager:debug("failed to find prompt ~s in ~p, using default"
                       ,[PromptId, language_keys(Language)]
                       ),
            default_prompt_path(PromptId, Language);
        'undefined' ->
            lager:debug("failed to find prompt ~s in ~p", [PromptId, language_keys(Language)]),
            case kz_services_reseller:get_id(AccountId) of
                AccountId -> default_prompt_path(PromptId, Language);
                ResellerId -> prompt_path(ResellerId, PromptId, L)
            end;
        Path -> Path
    end.

-spec default_prompt_path(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_binary().
default_prompt_path(PromptId, Language) ->
    #media_map{languages=Langs} = get_map(PromptId),
    lager:debug("checking default langs ~p", [default_language_keys(Language)]),
    kz_json:get_first_defined(default_language_keys(Language), Langs).

-spec handle_media_doc(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_media_doc(JObj, _Props) ->
    'true' = kapi_conf:doc_update_v(JObj),

    handle_media_doc_change(JObj, kz_json:get_value(<<"Event-Name">>, JObj)).

-spec handle_media_doc_change(kz_json:object(), kz_term:ne_binary()) -> 'ok'.
handle_media_doc_change(JObj, ?DOC_DELETED) ->
    MediaId = kz_json:get_value(<<"ID">>, JObj),
    PromptId = extract_prompt_id(MediaId),
    Database = kz_json:get_value(<<"Database">>, JObj),
    gen_listener:cast(?SERVER, {'rm_mapping', Database, PromptId});
handle_media_doc_change(JObj, _Change) ->
    Db = kz_json:get_value(<<"Database">>, JObj),
    {'ok', Doc} = kz_datamgr:open_doc(Db, kz_json:get_value(<<"ID">>, JObj)),
    gen_listener:cast(?MODULE, {'add_mapping', Db, Doc}).

%%------------------------------------------------------------------------------
%% @doc Parse the kz_media_map prompt ID from media doc ID.
%% Keeping old format separator `%2F' for backwards compatibility,
%% i.e. deleting prompts carried forward from Kazoo lower than 4.0.
%% @end
%%------------------------------------------------------------------------------
-spec extract_prompt_id(kz_term:ne_binary()) -> kz_term:ne_binary().
extract_prompt_id(<<_Lang:5/binary, "/", PromptId/binary>>) -> PromptId;
extract_prompt_id(<<_Lang:2/binary, "/", PromptId/binary>>) -> PromptId;
extract_prompt_id(<<_Lang:5/binary, "%2F", PromptId/binary>>) -> PromptId;
extract_prompt_id(<<_Lang:2/binary, "%2F", PromptId/binary>>) -> PromptId;
extract_prompt_id(MediaId) -> MediaId.

-spec table_id() -> ?MODULE.
table_id() -> ?MODULE.

-spec table_options() -> kz_term:proplist().
table_options() ->
    ['set'
    ,'protected'
    ,{'keypos', #media_map.id}
    ,'named_table'
    ].

-spec find_me_function() -> kz_term:api_pid().
find_me_function() -> whereis(?SERVER).

-spec gift_data() -> 'ok'.
gift_data() -> 'ok'.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call({'add_mapping', ?KZ_MEDIA_DB, JObj}, _From, State) ->
    _ = maybe_add_prompt(?KZ_MEDIA_DB, JObj),
    {'reply', 'ok', State};
handle_call({'add_mapping', Account, JObj}, _From, State) ->
    _ = maybe_add_prompt(kz_util:format_account_id(Account), JObj),
    {'reply', 'ok', State};
handle_call({'rm_mapping', ?KZ_MEDIA_DB, PromptId}, _From, State) ->
    lager:debug("removing prompt mappings for ~s/~s", [?KZ_MEDIA_DB, PromptId]),
    {'reply', ets:delete(table_id(), mapping_id(?KZ_MEDIA_DB, PromptId)), State};
handle_call({'rm_mapping', Account, PromptId}, _From, State) ->
    AccountId = kz_util:format_account_id(Account),
    lager:debug("removing prompt mappings for ~s/~s", [AccountId, PromptId]),
    {'reply', ets:delete(table_id(), mapping_id(AccountId, PromptId)), State};
handle_call({'new_map', Map}, _From, State) ->
    {'reply', ets:insert_new(table_id(), Map), State};
handle_call({'insert_map', Map}, _From, State) ->
    {'reply', ets:insert(table_id(), Map), State};
handle_call(_Request, _From, State) ->
    lager:debug("unhandled call: ~p", [_Request]),
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast('flush', State) ->
    ets:delete_all_objects(table_id()),
    lager:debug("flushed all media mappings"),
    _ = kz_process:spawn(fun init_map/0),
    {'noreply', State};
handle_cast({'add_mapping', ?KZ_MEDIA_DB, JObj}, State) ->
    _ = maybe_add_prompt(?KZ_MEDIA_DB, JObj),
    {'noreply', State};
handle_cast({'add_mapping', AccountId, JObj}, State) ->
    _ = maybe_add_prompt(kz_util:format_account_id(AccountId), JObj),
    {'noreply', State};
handle_cast({'rm_mapping', ?KZ_MEDIA_DB, PromptId}, State) ->
    lager:debug("removing prompt mappings for ~s/~s", [?KZ_MEDIA_DB, PromptId]),
    ets:delete(table_id(), mapping_id(?KZ_MEDIA_DB, PromptId)),
    {'noreply', State};
handle_cast({'rm_mapping', Account, PromptId}, State) ->
    AccountId = kz_util:format_account_id(Account),
    lager:debug("removing prompt mappings for ~s/~s", [AccountId, PromptId]),
    ets:delete(table_id(), mapping_id(AccountId, PromptId)),
    {'noreply', State};
handle_cast({'gen_listener', {'created_queue', _QueueNAme}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener', {'is_consuming', _IsConsuming}}, State) ->
    {'noreply', State};
handle_cast({'gen_listener',{'federators_consuming', _AreFederatorsConsuming}}, State) ->
    {'noreply', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'ETS-TRANSFER', _TableId, _From, _GiftData}, State) ->
    lager:debug("recv control of ~p from ~p", [_TableId, _From]),
    _ = kz_process:spawn(fun init_map/0),
    {'noreply', State};
handle_info(_Info, State) ->
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init_map() -> 'ok'.
init_map() ->
    init_map(?KZ_MEDIA_DB).

-spec init_map(kz_term:ne_binary()) -> 'ok'.
init_map(Db) ->
    init_map(Db, <<"media/crossbar_listing">>, <<>>, 50, fun gen_listener:cast/2).

-spec init_map(kz_term:ne_binary(), kz_term:ne_binary(), binary(), pos_integer(), fun()) -> 'ok'.
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

-spec init_map(kz_term:ne_binary(), kz_term:ne_binary(), binary(), pos_integer(), fun(), kz_json:objects()) -> 'ok'.
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

-spec add_mapping(kz_term:ne_binary(), fun(), kz_json:objects()) -> 'ok'.
add_mapping(Db, SendFun, JObjs) ->
    add_mapping(Db, SendFun, JObjs, whereis(?MODULE)).

-spec add_mapping(kz_term:ne_binary(), fun(), kz_json:objects(), pid()) -> 'ok'.
add_mapping(Db, _SendFun, JObjs, Srv) when Srv =:= self() ->
    AccountId = kz_util:format_account_id(Db, 'raw'),
    _ = [maybe_add_prompt(AccountId, kz_json:get_value(<<"doc">>, JObj))
         || JObj <- JObjs
        ],
    'ok';
add_mapping(Db, SendFun, JObjs, Srv) ->
    AccountId = kz_util:format_account_id(Db, 'raw'),
    _ = [SendFun(Srv, {'add_mapping', AccountId, kz_json:get_value(<<"doc">>, JObj)}) || JObj <- JObjs],
    'ok'.

-spec maybe_add_prompt(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
maybe_add_prompt(AccountId, JObj) ->
    maybe_add_prompt(AccountId
                    ,JObj
                    ,kz_json:get_first_defined([<<"prompt_id">>
                                               ,[<<"doc">>, <<"prompt_id">>]
                                               ]
                                              ,JObj
                                              )
                    ).

-spec maybe_add_prompt(kz_term:ne_binary(), kz_json:object(), kz_term:api_binary()) -> 'ok'.
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
    Languages = kz_json:set_value(Lang
                                 ,kz_media_util:prompt_path(kz_doc:account_id(JObj, ?KZ_MEDIA_DB)
                                                           ,kz_http_util:urlencode(kz_doc:id(JObj))
                                                           )
                                 ,Langs
                                 ),
    UpdatedMap = Map#media_map{account_id=AccountId
                              ,prompt_id=PromptId
                              ,languages=Languages
                              },

    insert_map(UpdatedMap).

-spec insert_map(media_map()) -> 'ok' | 'true'.
insert_map(Map) ->
    insert_map(Map, whereis(?MODULE)).

-spec insert_map(media_map(), pid()) -> 'ok' | 'true'.
insert_map(Map, Srv) when Srv =:= self() ->
    ets:insert(table_id(), Map);
insert_map(Map, Srv) ->
    gen_listener:call(Srv, {'insert_map', Map}).

-spec get_map(kz_term:ne_binary()) -> media_map().
get_map(PromptId) ->
    get_map(?KZ_MEDIA_DB, PromptId).

-spec get_map(kz_term:ne_binary(), kz_term:ne_binary()) -> media_map().
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

-spec init_account_map(kz_term:ne_binary(), kz_term:ne_binary()) -> 'true'.
init_account_map(AccountId, PromptId) ->
    SystemMap = get_map(PromptId),
    MapId = mapping_id(AccountId, PromptId),

    AccountMap = SystemMap#media_map{id=MapId
                                    ,account_id=AccountId
                                    },
    new_map(AccountMap).

-spec new_map(media_map()) -> 'true'.
new_map(Map) ->
    new_map(Map, whereis(?MODULE)).

-spec new_map(media_map(), pid()) -> 'true'.
new_map(Map, Srv) when Srv =:= self() ->
    ets:insert_new(table_id(), Map);
new_map(Map, Srv) ->
    'true' = gen_listener:call(Srv, {'new_map', Map}).

-spec load_account_map(kz_term:ne_binary(), kz_term:ne_binary()) -> media_map().
load_account_map(AccountId, PromptId) ->
    lager:debug("attempting to load account map for ~s/~s", [AccountId, PromptId]),

    case kz_datamgr:get_results(kz_util:format_account_id(AccountId, 'encoded')
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

-spec default_language_keys(kz_term:ne_binary()) -> kz_term:ne_binaries().
default_language_keys(Language) ->
    DefaultLanguage = kz_media_util:default_prompt_language(),
    language_keys(Language) ++ [DefaultLanguage].

-spec language_keys(kz_term:ne_binary()) -> kz_term:ne_binaries().
language_keys(Language) ->
    language_keys(Language, []).

-spec language_keys(kz_term:ne_binary(), kz_term:ne_binaries()) -> kz_term:ne_binaries().
language_keys(<<Primary:2/binary>>, Acc) ->
    lists:reverse([Primary | Acc]);
language_keys(<<Primary:2/binary, "-", _Secondary:2/binary>> = Lang, Acc) ->
    language_keys(Primary, [Lang | Acc]);
language_keys(<<Primary:5/binary, "_", _Secondary:5/binary>> = Lang, Acc) ->
    language_keys(Primary, [Lang | Acc]);
language_keys(Lang, Acc) ->
    lists:reverse([Lang | Acc]).

-spec mapping_id(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
mapping_id(AccountId, PromptId) ->
    list_to_binary([AccountId, "/", PromptId]).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

language_keys_test_() ->
    LangTests = [{<<"en">>, [<<"en">>]}
                ,{<<"en-us">>, [<<"en-us">>, <<"en">>]}
                ,{<<"en-us_fr-fr">>, [<<"en-us_fr-fr">>, <<"en-us">>, <<"en">>]}
                ,{<<"foo-bar">>, [<<"foo-bar">>]}
                ],
    [?_assertEqual(Result, language_keys(Lang))
     || {Lang, Result} <- LangTests
    ].

-endif.
