%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%% Account module
%%%
%%% Handle client requests for account documents
%%%
%%% @end
%%% Created : 05 Jan 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(accounts).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).
-import(crossbar_util).

-include("../crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(SERVER, ?MODULE).

-define(ACCOUNTS_DB, "accounts").
-define(ACCOUNTS_LIST, {"accounts","listing"}).
-define(ACCOUNTS_PARENT, {"accounts","parent"}).
-define(ACCOUNTS_CHILDREN, {"accounts","children"}).
-define(ACCOUNTS_DESCENDANTS, {"accounts","descendants"}).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
-spec(init/1 :: (_) -> tuple(ok, #state{})).
init([]) ->
    bind_to_crossbar(),
    couch_mgr:load_doc_from_file(?ACCOUNTS_DB, crossbar, "accounts.json"),
    {ok, #state{}}.

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
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_cast(_Msg, State) ->
    io:format("Unhandled ~p", [_Msg]),
    {noreply, State}.

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.accounts">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.accounts">>, Payload}, State) ->
    spawn(fun() ->
		  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                crossbar_util:binding_heartbeat(Pid),
                Context1 = Context#cb_context{db_name=?ACCOUNTS_DB},
                Context2 =
                    case length(Context1#cb_context.req_nouns) of
                        1 ->
                            validate(wrq:method(RD), Params, Context1);
                      _ ->
                            load_account(Params, Context1)
                    end,
                Pid ! {binding_result, true, [RD, Context2, Params]}
	 end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.accounts">>, [RD, Context | [_, <<"parent">>]=Params]}, State) ->
    spawn(fun() ->
                  crossbar_util:binding_heartbeat(Pid),
                  case crossbar_doc:save(Context#cb_context{db_name=?ACCOUNTS_DB}) of
                      #cb_context{resp_status=success}=Context1 ->
                          Pid ! {binding_result, true, [RD, Context1#cb_context{resp_data={struct, []}}, Params]};
                      Else ->
                        Pid ! {binding_result, true, [RD, Else, Params]}
                  end
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  Context1 = crossbar_doc:save(Context#cb_context{db_name=?ACCOUNTS_DB}),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
	  end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.accounts">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  case crossbar_doc:save(Context#cb_context{db_name=?ACCOUNTS_DB}) of
                      #cb_context{resp_status=success, doc=Doc}=Context1 ->
                          couch_mgr:db_info(get_db_name(Doc)),
                          Pid ! {binding_result, true, [RD, Context1, Params]};
                      Else ->
                        Pid ! {binding_result, true, [RD, Else, Params]}
                  end
	  end),
    {noreply, State};

%handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.accounts">>, [RD, #cb_context{doc=Doc}=Context | [_, <<"parent">>]=Params]}, State) ->
%    %%spawn(fun() ->
%                  Doc1 = crossbar_util:set_json_values([<<"pvt_identifier">>, <<"tree">>], [], Doc),
%                  Context1 = crossbar_doc:save(Context#cb_context{db_name=?ACCOUNTS_DB, doc=Doc1}),
%                  Pid ! {binding_result, true, [RD, Context1, Params]},
%	%%  end),
%    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.accounts">>, [RD, Context | Params]}, State) ->
    %%spawn(fun() ->
                  Context1 = crossbar_doc:delete(Context#cb_context{db_name=?ACCOUNTS_DB}),
                  Pid ! {binding_result, true, [RD, Context1, Params]},
	%%  end),
    {noreply, State};

handle_info({binding_fired, Pid, _Route, Payload}, State) ->
    %%format_log(info, "ACCOUNT(~p): unhandled binding: ~p~n", [self(), Route]),
    Pid ! {binding_result, true, Payload},
    {noreply, State};

handle_info(_Info, State) ->
    format_log(info, "ACCOUNT(~p): unhandled info ~p~n", [self(), _Info]),
    {noreply, State}.

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
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function binds this server to the crossbar bindings server,
%% for the keys we need to consume.
%% @end
%%--------------------------------------------------------------------
-spec(bind_to_crossbar/0 :: () -> no_return()).
bind_to_crossbar() ->
    crossbar_bindings:bind(<<"v1_resource.allowed_methods.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.resource_exists.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.validate.accounts">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.accounts">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec(allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), http_methods())).
allowed_methods([]) ->
    {true, ['GET', 'PUT']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([_, <<"parent">>]) ->
    {true, ['GET', 'POST', 'DELETE']};
allowed_methods([_, Path]) ->
    Valid = lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>]),
    {Valid, ['GET']};
allowed_methods(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec(resource_exists/1 :: (Paths :: list()) -> tuple(boolean(), [])).
resource_exists([]) ->
    {true, []};
resource_exists([_]) ->
    {true, []};
resource_exists([_, Path]) ->
    Valid = lists:member(Path, [<<"parent">>, <<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>]),
    {Valid, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec(validate/3 :: (Verb :: http_method(), Params :: list(), Context :: #cb_context{}) -> #cb_context{}).
validate('GET', [], Context) ->
    crossbar_doc:load_view(?ACCOUNTS_LIST, [], Context, fun filter_view_results/2);
validate('PUT', [], #cb_context{req_data={struct, [_]}=Data}=Context) ->
    case is_valid_doc(Data) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            create_doc(Data, Context)
    end;
validate('GET', [DocId], Context) ->
    crossbar_doc:load(DocId, Context);
validate('POST', [DocId], #cb_context{req_data={struct,[_]}=Data}=Context) ->
    case is_valid_doc(Data) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            crossbar_doc:load_merge(DocId, Data, Context)
    end;
validate('DELETE', [DocId], Context) ->
    crossbar_doc:load(DocId, Context);
validate('GET', [DocId, <<"parent">>], Context) ->
    Child =
        crossbar_doc:load_view(?ACCOUNTS_PARENT, [
             {<<"startkey">>, DocId}
            ,{<<"endkey">>, DocId}
        ], Context),
    case Child#cb_context.doc of
        [{struct, Props}|_] ->
            Parent = proplists:get_value(<<"value">>, Props),
            crossbar_doc:load_view(?ACCOUNTS_LIST, [
                 {<<"startkey">>, [Parent]}
                ,{<<"endkey">>, [Parent, {struct, []}]}
            ], Context, fun filter_view_results/2);
        _Else ->
            Context
    end;
validate('POST', [DocId, <<"parent">>], #cb_context{req_data={struct,Data}}=Context) ->
    case is_valid_parent(Data) of
        {false, Fields} ->
            crossbar_util:response_invalid_data(Fields, Context);
        {true, []} ->
            %% OMGBBQ! NO CHECKS FOR CYCLIC REFERENCES WATCH OUT!
            ParentId = proplists:get_value(<<"parent">>, Data),
            update_tree(DocId, ParentId, Context)
    end;
validate('DELETE', [DocId, <<"parent">>], Context) ->
    crossbar_doc:load(DocId, Context);
validate('GET', [DocId, <<"children">>], Context) ->
    crossbar_doc:load_view(?ACCOUNTS_CHILDREN, [
         {<<"startkey">>, [DocId]}
        ,{<<"endkey">>, [DocId, {struct, []}]}
    ], Context, fun filter_view_results/2);
validate('GET', [DocId, <<"descendants">>], Context) ->
    crossbar_doc:load_view(?ACCOUNTS_DESCENDANTS, [
         {<<"startkey">>, [DocId]}
        ,{<<"endkey">>, [DocId, {struct, []}]}
    ], Context, fun filter_view_results/2);
validate('GET', [DocId, <<"siblings">>], Context) ->
    Context1 =
        crossbar_doc:load_view(?ACCOUNTS_PARENT, [
             {<<"startkey">>, DocId}
            ,{<<"endkey">>, DocId}
        ], Context),
    case Context1#cb_context.doc of
        [{struct, Props}|_] ->
            Parent = proplists:get_value(<<"value">>, Props),
            crossbar_doc:load_view(?ACCOUNTS_CHILDREN, [
                 {<<"startkey">>, [Parent]}
                ,{<<"endkey">>, [Parent, {struct, []}]}
            ], Context, fun filter_view_results/2);
        _Else ->
            Context
    end;
validate(_, _, Context) ->
    crossbar_util:response_faulty_request(Context).

filter_view_results({struct, Prop}, Acc) ->
    Id = proplists:get_value(<<"id">>, Prop),
    Name = proplists:get_value(<<"value">>, Prop),
    Level = length(lists:nth(2, proplists:get_value(<<"key">>, Prop))),
    [{struct,[
        {<<"id">>, Id}
       ,{<<"level">>, Level}
       ,{<<"name">>, Name}
    ]} | Acc].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function creates a new account document with the given data
%% @end
%%--------------------------------------------------------------------
-spec(create_doc/2 :: (Data :: json_object(), Context :: #cb_context{}) -> #cb_context{}).
create_doc({struct, Data}, Context) ->
    Doc = create_private_fields() ++ Data,
    Context#cb_context{
         doc={struct, Doc}
        ,resp_status=success
    }.


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_valid_parent/1 :: (Data :: proplist()) -> tuple(boolean(), list())).
is_valid_parent(_Data) ->
    {true, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(is_valid_doc/1 :: (Data :: json_object()) -> tuple(boolean(), json_objects())).
is_valid_doc({struct, Data}) ->
    Schema = [
	   { [<<"base">>, <<"name">>]
	    ,[ {not_empty, []}
              ,{is_format, [phrase]}
	     ]}
	   ,{ [<<"base">>, <<"status">>]
	      ,[ {not_empty, []}
		%,{in_list, [{<<"enabled">>, <<"disabled">>}]}
	       ]}
	  ],
    Failed = crossbar_validator:validate(Schema, Data),
    {Failed =:= [], Failed}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(update_tree/3 :: (DocId :: binary(), ParentId :: binary(), Context :: #cb_context{}) -> #cb_context{}).
update_tree(DocId, ParentId, Context) ->
    case crossbar_doc:load(ParentId, Context) of
        #cb_context{resp_status=success, doc=Parent} ->
            Descendants =
                crossbar_doc:load_view(?ACCOUNTS_DESCENDANTS, [
                     {<<"startkey">>, [DocId]}
                    ,{<<"endkey">>, [DocId, {struct, []}]}
                ], Context),
            case Descendants of
                #cb_context{resp_status=success, doc=[]} ->
                    crossbar_util:response_bad_identifier(DocId, Context);
                #cb_context{resp_status=success, doc=Doc}=Context1 ->
                    Tree = crossbar_util:get_json_values([<<"pvt_identifier">>, <<"tree">>], Parent) ++ [ParentId, DocId],
                    Updater = fun(Update, Acc) -> update_doc_tree(Tree, Update, Acc) end,
                    %%Updates = plists:fold(Updater, {recursive, fun(R1, R2) -> R1 ++ R2 end}, [], Doc, 1),
                    Updates = lists:foldr(Updater, [], Doc),
                    Context1#cb_context{doc=Updates}
            end;
        Else ->
            Else
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec(update_doc_tree/3 :: (ParentTree :: list(), Update :: json_object(), Acc :: json_objects()) -> json_objects()).
update_doc_tree(ParentTree, {struct, Prop}, Acc) ->
    DocId = proplists:get_value(<<"id">>, Prop),
    ParentId = lists:last(ParentTree),
    case crossbar_doc:load(DocId, #cb_context{db_name=?ACCOUNTS_DB}) of
        #cb_context{resp_status=success, doc=Doc} ->
            Tree = crossbar_util:get_json_values([<<"pvt_identifier">>, <<"tree">>], Doc),
            SubTree =
                case lists:dropwhile(fun(E)-> E =/= ParentId end, Tree) of
                    [] -> [];
                    List -> lists:nthtail(1,List)
                end,
            NewTree = lists:filter(fun(E) -> E =/= DocId end, ParentTree ++ SubTree),
            %%io:format("Tree:~p~nSubTree: ~p~nNewTree:~p~n:Doc:~p~n", [Tree, SubTree, NewTree, crossbar_util:set_json_values([<<"pvt_identifier">>, <<"tree">>], NewTree, Doc)]),
            [crossbar_util:set_json_values([<<"pvt_identifier">>, <<"tree">>], NewTree, Doc) | Acc];
        _Else ->
            Acc
    end;
update_doc_tree(_ParentTree, _Object, Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns the private fields to be added to a new account
%% document
%% @end
%%--------------------------------------------------------------------
-spec(create_private_fields/0 :: () -> proplist()).
create_private_fields() ->
    [
        {<<"pvt_identifier">>,{struct,[{<<"type">>,<<"account">>},{<<"tree">>,[]}]}}
    ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to convert the account document ID into
%% the account database name
%% @end
%%--------------------------------------------------------------------
-spec(get_db_name/1 :: (Doc :: json_object()) -> undefined | binary()).
get_db_name({struct, Doc}) ->
    case proplists:get_value(<<"_id">>, Doc) of
        undefined ->
            undefined;
       _Id ->
            Id = whistle_util:to_list(_Id),
            Db = [string:sub_string(Id, 1, 2), "%2F", string:sub_string(Id, 3, 4), "%2F", string:sub_string(Id, 5)],
            whistle_util:to_binary(Db)
    end.

load_account([DocId|_], Context) ->
    case crossbar_doc:load(DocId, Context) of
        #cb_context{resp_status=success, doc=Doc}=Context1 ->
            Context1#cb_context{
               db_name = get_db_name(Doc)
              ,resp_status = error
              ,resp_error_msg = undefined
              ,resp_error_code = undefined
              ,resp_data = []
            };
        Else ->
            Else#cb_context{db_name=undefined}
    end;
load_account(_, Context) ->
    crossbar_util:response_db_missing(Context#cb_context{db_name=undefined}).