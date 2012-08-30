%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% CRUD for call queues
%%% /queues
%%%   GET: list all known queues
%%%   PUT: create a new queue
%%%
%%% /queues/stats
%%%   GET: retrieve stats across all queues
%%%
%%% /queues/QID
%%%   GET: queue details
%%%   POST: edit queue details
%%%   DELETE: delete a queue
%%%
%%% /queues/QID/stats
%%%   GET: retrieve stats for this queue
%%% /queues/QID/roster
%%%   GET: get list of agent_ids
%%%   POST: add a list of agent_ids
%%%   DELETE: rm a list of agent_ids
%%%
%%% @end
%%% @contributors:
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_queues).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1
         ,post/2, post/3
         ,delete/2, delete/3
        ]).

-include("include/crossbar.hrl").

-define(PVT_TYPE, <<"queue">>).
-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(CB_LIST, <<"queues/crossbar_listing">>).
-define(CB_AGENTS_LIST, <<"queues/agents_listing">>). %{agent_id, queue_id}

-define(STATS_PATH_TOKEN, <<"stats">>).
-define(ROSTER_PATH_TOKEN, <<"roster">>).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Initializes the bindings this module will respond to.
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.queues">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.queues">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.queues">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.queues">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.queues">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.queues">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given the path tokens related to this module, what HTTP methods are
%% going to be responded to.
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), path_token()) -> http_methods().
allowed_methods() ->
    ['GET', 'PUT'].
allowed_methods(?STATS_PATH_TOKEN) ->
    ['GET'];
allowed_methods(_QID) ->
    ['GET', 'POST', 'DELETE'].

allowed_methods(_QID, ?STATS_PATH_TOKEN) ->
    ['GET'];
allowed_methods(_QID, ?ROSTER_PATH_TOKEN) ->
    ['GET', 'POST', 'DELETE'].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Does the path point to a valid resource
%% So /queues => []
%%    /queues/foo => [<<"foo">>]
%%    /queues/foo/bar => [<<"foo">>, <<"bar">>]
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_token()) -> 'true'.
-spec resource_exists/2 :: (path_token(), path_token()) -> 'true'.
resource_exists() -> true.
resource_exists(_) -> true.
resource_exists(_, ?STATS_PATH_TOKEN) -> true;
resource_exists(_, ?ROSTER_PATH_TOKEN) -> true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Check the request (request body, query string params, path tokens, etc)
%% and load necessary information.
%% /queues mights load a list of queue objects
%% /queues/123 might load the queue object 123
%% Generally, use crossbar_doc to manipulate the cb_context{} record
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
validate(#cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate(#cb_context{req_verb = <<"put">>}=Context) ->
    create(Context).

validate(#cb_context{req_verb = <<"get">>}=Context, ?STATS_PATH_TOKEN) ->
    %% TODO: fetch stats across all queues in this account
    Context;
validate(#cb_context{req_verb = <<"get">>}=Context, Id) ->
    read(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id) ->
    update(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id) ->
    read(Id, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?STATS_PATH_TOKEN) ->
    %% TODO: fetch stats for this queue
    Context;
validate(#cb_context{req_verb = <<"get">>}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    %% TODO: get the list of agents in this queue
    load_agent_roster(Id, Context);
validate(#cb_context{req_verb = <<"post">>}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    %% TODO: add queue_id to agent docs
    add_queue_to_agents(Id, Context);
validate(#cb_context{req_verb = <<"delete">>}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    %% TODO: remove queue_id from agent docs
    rm_queue_from_agents(Id, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is PUT, execute the actual action, usually a db save.
%% @end
%%--------------------------------------------------------------------
-spec put/1 :: (#cb_context{}) -> #cb_context{}.
put(#cb_context{}=Context) ->
    lager:debug("saving new queue"),
    crossbar_doc:save(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is POST, execute the actual action, usually a db save
%% (after a merge perhaps).
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec post/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
post(#cb_context{}=Context, _) ->
    crossbar_doc:save(Context).
post(#cb_context{}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    read(Id, crossbar_doc:save(Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% If the HTTP verib is DELETE, execute the actual action, usually a db delete
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec delete/3 :: (#cb_context{}, path_token(), path_token()) -> #cb_context{}.
delete(#cb_context{}=Context, _) ->
    crossbar_doc:delete(Context).
delete(#cb_context{}=Context, Id, ?ROSTER_PATH_TOKEN) ->
    read(Id, crossbar_doc:save(Context)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#cb_context{}) -> #cb_context{}.
create(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"queues">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            Context#cb_context{doc=JObj1, resp_status=success}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an instance from the database
%% @end
%%--------------------------------------------------------------------
-spec read/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
read(Id, Context) ->
    case crossbar_doc:load(Id, Context) of
        #cb_context{resp_status=success}=Context1 ->
            load_queue_agents(Id, Context1);
        Context1 -> Context1
    end.

load_queue_agents(Id, #cb_context{resp_data=Queue}=Context) ->
    case load_agent_roster(Id, Context) of
        #cb_context{resp_status=success, resp_data=Agents} ->
            Context#cb_context{resp_data=wh_json:set_value(<<"agents">>, Agents, Queue)};
        _ -> Context
    end.

load_agent_roster(Id, Context) ->
    crossbar_doc:load_view(?CB_AGENTS_LIST, [{key, Id}]
                           ,Context
                           ,fun normalize_agents_results/2
                          ).

add_queue_to_agents(Id, #cb_context{req_data=[_|_]=AgentIds}=Context) ->
    case crossbar_doc:load(AgentIds, Context) of
        #cb_context{resp_status=success
                    ,doc=Agents
                   }=Context1 ->
            lager:debug("loaded agents for queue ~s: ~p", [Id, Agents]),
            Context1#cb_context{doc=[maybe_add_queue_to_agent(Id, A) || A <- Agents]};
        Context1 -> Context1
    end.

maybe_add_queue_to_agent(Id, A) ->
    Qs = case wh_json:get_value(<<"queues">>, A) of
             L when is_list(L) ->
                 case lists:member(Id, L) of
                     true -> L;
                     false -> [Id | L]
                 end;
             _ -> [Id]
         end,
    wh_json:set_value(<<"queues">>, Qs, A).

rm_queue_from_agents(Id, #cb_context{req_data=[_|_]=AgentIds}=Context) ->
    case crossbar_doc:load(AgentIds, Context) of
        #cb_context{resp_status=success
                    ,doc=Agents
                   }=Context1 ->
            lager:debug("loaded agents for queue ~s: ~p", [Id, Agents]),
            Context1#cb_context{doc=[maybe_rm_queue_from_agent(Id, A) || A <- Agents]};
        Context1 -> Context1
    end.

maybe_rm_queue_from_agent(Id, A) ->
    Qs = wh_json:get_value(<<"queues">>, A, []),
    wh_json:set_value(<<"queues">>, lists:delete(Id, Qs), A).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(Id, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"queues">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = lists:foldr(fun(F, {J, C}) ->
                                             {F(J, C), C}
                                     end, {JObj, Context}, ?PVT_FUNS),
            crossbar_doc:load_merge(Id, JObj1, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary/1 :: (#cb_context{}) -> #cb_context{}.
summary(Context) ->
    crossbar_doc:load_view(?CB_LIST, [], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

-spec normalize_agents_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_agents_results(JObj, Acc) ->
    [wh_json:get_value(<<"id">>, JObj) | Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (wh_json:json_object(), #cb_context{}) -> wh_json:json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj).
