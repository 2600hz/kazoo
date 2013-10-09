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
-export([process_number/1]).
-export([emergency_cid/1, emergency_cid/2, emergency_cid/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Flush the stepswitch local cache
%% @end
%%--------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() -> wh_cache:flush_local(?STEPSWITCH_CACHE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    lager:debug("ensuring database ~s exists", [?RESOURCES_DB]),
    couch_mgr:db_create(?RESOURCES_DB),
    Views = whapps_util:get_views_json('stepswitch', "views"),
    whapps_util:update_views(?RESOURCES_DB, Views, 'true'),
    case catch couch_mgr:all_docs(?RESOURCES_DB, ['include_docs']) of
        {'error', _} -> 'ok';
        {'EXIT', _E} ->
            lager:debug("failure looking up all docs in ~s: ~p", [?RESOURCES_DB, _E]);
        {'ok', JObjs} ->
            _ = couch_mgr:del_docs(?RESOURCES_DB
                                   ,[Doc
                                     || JObj <- JObjs,
                                        begin
                                            Doc = wh_json:get_value(<<"doc">>, JObj),
                                            wh_json:get_value(<<"pvt_type">>, Doc) =:= <<"route">>
                                        end
                                    ]),
            'ok'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec lookup_number(string()) ->
                           {'ok', binary()} |
                           {'error', atom()}.
lookup_number(Number) ->
    stepswitch_util:stepswitch_util(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Instructs stepswitch_outbound to re-scan the resource db and
%% refresh the cache.
%% @end
%%--------------------------------------------------------------------
-spec reload_resources() -> 'ok'.
reload_resources() -> 'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns a list of tuples that represent the routing logic for the
%% provided number and flags.  The tuple containts:
%% {Resource ID, Delay (in seconds), SIP URI}
%% @end
%%--------------------------------------------------------------------
-spec process_number(string()) -> list() | {'error', atom()}.
process_number(Number) ->
    stepswitch_resources:get_endpoints(Number, wh_json:new()).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec emergency_cid(ne_binary()) -> 'ok'.
-spec emergency_cid(ne_binary(), text()) -> 'ok'.
-spec emergency_cid(ne_binary(), 'undefined' | text(), text()) -> 'ok'.

emergency_cid(Account) -> emergency_cid(Account, 'undefined').
emergency_cid(Account, ECID) -> emergency_cid(Account, ECID, 'undefined').
emergency_cid(Account, ECID, OCID) ->
    wh_cache:flush(),
    Props = [{<<"Account-ID">>, wh_util:to_binary(Account)}
             ,{<<"Emergency-Caller-ID-Number">>, ECID}
             ,{<<"Outbound-Caller-ID-Number">>, OCID}
            ],
    JObj = wh_json:from_list(props:filter_empty(Props)),
    CID = stepswitch_outbound:get_emergency_cid_number(JObj),
    lager:info("Emergency offnet requests for account ~s (given the following CIDs ~s and ~s) will use ~s", [Account, ECID, OCID, CID]).
