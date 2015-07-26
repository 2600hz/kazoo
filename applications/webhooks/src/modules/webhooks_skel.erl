%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz INC
%%%
%%% @contributors
%%%-------------------------------------------------------------------

-module(webhooks_skel).

-export([init/0
         ,bindings_and_routing/0
         ,handle_event/2
        ]).

-include("../webhooks.hrl").

-define(ID, ?MODULE).
-define(NAME, <<"skel">>).
-define(DESC, <<"Example webhook module">>).

-spec init() -> 'ok'.
-spec init(ne_binary()) -> 'ok'.
init() ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    init(MasterAccountDb).

init(MasterAccountDb) ->
    case metadata_exists(MasterAccountDb) of
        'true' -> lager:debug("~s already exists, not loading", [?MODULE]);
        'false' -> load_metadata(MasterAccountDb)
    end.

-spec metadata_exists(ne_binary()) -> boolean().
metadata_exists(MasterAccountDb) ->
    case couch_mgr:open_cache_doc(MasterAccountDb, ?ID) of
        {'ok', _} -> 'true';
        {'error', _} -> 'false'
    end.

-spec load_metadata(ne_binary()) -> 'ok'.
load_metadata(MasterAccountDb) ->
    MetaData = wh_json:from_list([{<<"_id">>, ?ID}
                                  ,{<<"name">>, ?NAME}
                                  ,{<<"description">>, ?DESC}
                                 ]),
    case couch_mgr:save_doc(MasterAccountDb, MetaData) of
        {'ok', _Saved} ->
            lager:debug("~s initialized successfully", [?MODULE]);
        {'error', 'conflict'} ->
            lager:debug("~s loaded elsewhere", [?MODULE]);
        {'error', _E} ->
            lager:warning("failed to load metadata for ~s: ~p", [?MODULE, _E])
    end.

-spec bindings_and_routing() ->
                                  {gen_listener:bindings()
                                   ,gen_listener:responders()
                                  }.
bindings_and_routing() ->
    {[{'self', []}]
     ,[{{?MODULE, 'handle_event'}
        ,[{<<"category">>, <<"name">>}]
       }
      ]
    }.

-spec handle_event(wh_json:object(), wh_proplist()) -> any().
handle_event(JObj, _Props) ->
    wh_util:put_callid(JObj),
    lager:debug("event handled").
