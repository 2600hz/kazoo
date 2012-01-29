%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Preforms maintenance operations against the stepswitch dbs
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_maintenance).

-include("stepswitch.hrl").

%% API
-export([refresh/0]).
-export([lookup_number/1]).
-export([reload_resources/0]).
-export([process_number/1, process_number/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec refresh/0 :: () -> ok.
refresh() ->
    ?LOG_SYS("ensuring database ~s exists", [?RESOURCES_DB]),
    couch_mgr:db_create(?RESOURCES_DB),
    Views = whapps_util:get_views_json(stepswitch, "views"),
    whapps_util:update_views(?RESOURCES_DB, Views, true),
    case couch_mgr:all_docs(?RESOURCES_DB, [{<<"include_docs">>, true}]) of
        {ok, JObjs} ->
            [couch_mgr:del_doc(?RESOURCES_DB, wh_json:get_value(<<"doc">>, JObj)) 
             || JObj <- JObjs
                    ,wh_json:get_value([<<"doc">>, <<"pvt_type">>], JObj) =:= <<"route">>
            ],
            ok;
        {error, _} ->
            ok
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Lookup a number in the route db and return the account ID if known
%% @end
%%--------------------------------------------------------------------
-spec lookup_number/1 :: (string()) -> {ok, binary()} | {error, atom()}.
lookup_number(Number) ->
    {ok, Srv} = stepswitch_sup:listener(),
    gen_server:call(Srv, {lookup_number, Number}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Instructs stepswitch_outbound to re-scan the resource db and
%% refresh the cache.
%% @end
%%--------------------------------------------------------------------
-spec reload_resources/0 :: () -> ok.
reload_resources() ->
    {ok, Srv} = stepswitch_sup:listener(),
    gen_server:call(Srv, {reload_resrcs}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Returns a list of tuples that represent the routing logic for the
%% provided number and flags.  The tuple containts:
%% {Resource ID, Delay (in seconds), SIP URI}
%% @end
%%--------------------------------------------------------------------
-spec process_number/1 :: (string()) -> list() | {error, atom()}.
-spec process_number/2 :: (string(), list()) -> list() | {error, atom()}.

process_number(Number) ->
    {ok, Srv} = stepswitch_sup:listener(),
    gen_server:call(Srv, {process_number, Number}).

process_number(Number, Flags) ->
    {ok, Srv} = stepswitch_sup:listener(),
    gen_server:call(Srv, {process_number, Number, Flags}).
