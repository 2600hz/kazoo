%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%% Preforms maintenance operations against the stepswitch dbs
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(stepswitch_maintenance).

-include("stepswitch.hrl").

%% API
-export([flush/0]).
-export([refresh/0]).
-export([lookup_number/1]).
-export([reload_resources/0]).
-export([process_number/1, process_number/2]).
-export([emergency_cid/1, emergency_cid/2, emergency_cid/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flush the stepswitch local cache
%% @end
%%--------------------------------------------------------------------
-spec flush/0 :: () -> ok.
flush() ->
    wh_cache:flush_local(?STEPSWITCH_CACHE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> 'ok'.
refresh() ->
    lager:debug("ensuring database ~s exists", [?RESOURCES_DB]),
    couch_mgr:db_create(?RESOURCES_DB),
    Views = whapps_util:get_views_json(stepswitch, "views"),
    whapps_util:update_views(?RESOURCES_DB, Views, true),
    case catch couch_mgr:all_docs(?RESOURCES_DB, [include_docs]) of
        {ok, JObjs} ->
            _ = couch_mgr:del_docs(?RESOURCES_DB
                                   ,[Doc
                                     || JObj <- JObjs,
                                        begin
                                            Doc = wh_json:get_value(<<"doc">>, JObj),
                                            wh_json:get_value(<<"pvt_type">>, Doc) =:= <<"route">>
                                        end
                                    ]),
            ok;
        {error, _} ->
            ok;
        {'EXIT', _E} ->
            lager:debug("failure looking up all docs in ~s: ~p", [?RESOURCES_DB, _E]),
            ok
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec lookup_number/1 :: (string()) -> {'ok', binary()} | {'error', atom()}.
lookup_number(Number) ->
    gen_server:call(stepswitch_listener, {lookup_number, Number}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Instructs stepswitch_outbound to re-scan the resource db and
%% refresh the cache.
%% @end
%%--------------------------------------------------------------------
-spec reload_resources/0 :: () -> 'ok'.
reload_resources() ->
    gen_server:call(stepswitch_listener, {reload_resrcs}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns a list of tuples that represent the routing logic for the
%% provided number and flags.  The tuple containts:
%% {Resource ID, Delay (in seconds), SIP URI}
%% @end
%%--------------------------------------------------------------------
-spec process_number/1 :: (string()) -> list() | {'error', atom()}.
-spec process_number/2 :: (string(), list()) -> list() | {'error', atom()}.

process_number(Number) ->
    gen_server:call(stepswitch_listener, {process_number, Number}).

process_number(Number, Flags) ->
    gen_server:call(stepswitch_listener, {process_number, Number, Flags}).


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec emergency_cid/1 :: (ne_binary()) -> 'no_return'.
-spec emergency_cid/2 :: (ne_binary(), text()) -> 'no_return'.
-spec emergency_cid/3 :: (ne_binary(), 'undefined' | text(), text()) -> 'no_return'.

emergency_cid(Account) ->
    emergency_cid(Account, undefined).

emergency_cid(Account, ECID) ->
    emergency_cid(Account, ECID, undefined).

emergency_cid(Account, ECID, OCID) ->
    wh_cache:flush(),
    Props = [{<<"Account-ID">>, wh_util:to_binary(Account)}
             ,{<<"Emergency-Caller-ID-Number">>, ECID}
             ,{<<"Outgoing-Caller-ID-Number">>, OCID}
            ],
    JObj = wh_json:from_list(props:filter_empty(Props)),
    CID = stepswitch_outbound:get_emergency_cid_number(JObj),
    lager:info("Emergency offnet requests for account ~s (given the following CIDs ~s and ~s) will use ~s", [Account, ECID, OCID, CID]),
    no_return.
