%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Upload a rate deck, query rates for a given DID
%%% @end
%%% Created : 26 Jan 2012 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(cb_rates).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../../include/crossbar.hrl").

-define(SERVER, ?MODULE).
-define(PVT_FUNS, [fun add_pvt_type/2]).
-define(PVT_TYPE, <<"rate">>).
-define(CB_LIST, <<"rates/crossbar_listing">>).

-define(UPLOAD_MIME_TYPES, ["text/csv", "text/comma-separated-values"]).

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
init(_) ->
    _ = couch_mgr:db_create(?WH_RATES_DB),
    _ = couch_mgr:revise_doc_from_file(?WH_RATES_DB, crossbar, "views/rates.json"),
    _ = bind_to_crossbar(),
    {ok, ok}.

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.rates">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = allowed_methods(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.rates">>, Payload}, State) ->
    spawn(fun() ->
                  {Result, Payload1} = resource_exists(Payload),
                  Pid ! {binding_result, Result, Payload1}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.content_types_accepted.rates">>, {RD, Context, Params}}, State) ->
    spawn(fun() ->
                  Context1 = content_types_accepted(Params, Context),
                  Pid ! {binding_result, true, {RD, Context1, Params}}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.validate.rates">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),

                  Context1 = try
                                 validate(Params, Context#cb_context{db_name=?WH_RATES_DB})
                             catch
                                 _E:R ->
                                     ?LOG("failed to validate: ~p:~p", [_E, R]),
                                     crossbar_util:response(fatal, <<"exception encountered">>, 500, wh_json:new(), Context)
                             end,

                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.post.rates">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 =
                      case Context#cb_context.doc of
                          {0, _} ->
                              ?LOG("no rates to save"),
                              crossbar_util:response_invalid_data(wh_json:new(), Context);
                          {Count, Rates} when Count < 10 ->
                              ?LOG("only ~b rates to try saving", [Count]),
                              crossbar_doc:save(Context#cb_context{doc=Rates});
                          {Count, Rates} ->
                              ?LOG("there are ~b rates to save, responding with a 202 and saving in a spawn", [Count]),
                              _ = spawn(fun() ->
                                                _ = crossbar_util:put_reqid(Context),
                                                Now = erlang:now(),
                                                crossbar_doc:save(Context#cb_context{doc=Rates}, [{publish_doc, false}]),
                                                Elapsed = timer:now_diff(erlang:now(), Now),
                                                ?LOG("it took ~b micro, ~b milli, ~b sec to save ~b rates", [Elapsed, Elapsed div 1000, Elapsed div 1000000, Count])
                                        end),
                              crossbar_util:response_202(list_to_binary(["saving ", wh_util:to_list(Count), " rates from the uploaded document"]), Context);
                          _Rate ->
                              crossbar_doc:save(Context)
                      end,
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.put.rates">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = crossbar_doc:save(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.rates">>, [RD, Context | Params]}, State) ->
    spawn(fun() ->
                  _ = crossbar_util:put_reqid(Context),
                  crossbar_util:binding_heartbeat(Pid),
                  Context1 = crossbar_doc:delete(Context),
                  Pid ! {binding_result, true, [RD, Context1, Params]}
          end),
    {noreply, State};

handle_info({binding_fired, Pid, _, Payload}, State) ->
    Pid ! {binding_result, false, Payload},
    {noreply, State};

handle_info({binding_flushed, B}, State) ->
    ?LOG("binding ~s flushed", [B]),
    {noreply, State};

handle_info(_Info, State) ->
    ?LOG("unhandled message: ~p", [_Info]),
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
-spec bind_to_crossbar/0 :: () ->  'ok' | {'error', 'exists'}.
bind_to_crossbar() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.rates">>),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.rates">>),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.rates">>),
    _ = crossbar_bindings:bind(<<"v1_resource.content_types_accepted.rates">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.rates">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/1 :: (path_tokens()) -> {boolean(), http_methods()}.
allowed_methods([]) ->
    {true, ['GET', 'PUT', 'POST']};
allowed_methods([_]) ->
    {true, ['GET', 'POST', 'DELETE']};
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
-spec resource_exists/1 :: (path_tokens()) -> {boolean(), []}.
resource_exists([]) ->
    {true, []};
resource_exists([_]) ->
    {true, []};
resource_exists(_) ->
    {false, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add content types accepted and provided by this module
%%
%% @end
%%--------------------------------------------------------------------

%% -spec content_types_provided/2 :: (path_tokens(), #cb_context{}) -> #cb_context{}.
%% content_types_provided([], #cb_context{req_verb = <<"post">>}=Context) ->
%%     CTP = [{to_binary, ?UPLOAD_MIME_TYPES}],
%%     Context#cb_context{content_types_provided=CTP};
%% content_types_provided(_, Context) -> Context.

-spec content_types_accepted/2 :: (path_tokens(), #cb_context{}) -> #cb_context{}.
content_types_accepted([], #cb_context{req_verb = <<"post">>}=Context) ->
    Context#cb_context{content_types_accepted = [{from_binary, ?UPLOAD_MIME_TYPES}]};
content_types_accepted(_, Context) -> Context.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/2 :: (path_tokens(), #cb_context{}) -> #cb_context{}.
validate([], #cb_context{req_verb = <<"get">>}=Context) ->
    summary(Context);
validate([], #cb_context{req_verb = <<"put">>}=Context) ->
    create(Context);
validate([], #cb_context{req_verb = <<"post">>}=Context) ->
    check_uploaded_file(Context);
validate([Id], #cb_context{req_verb = <<"get">>}=Context) ->
    read(Id, Context);
validate([Id], #cb_context{req_verb = <<"post">>}=Context) ->
    update(Id, Context);
validate([Id], #cb_context{req_verb = <<"delete">>}=Context) ->
    read(Id, Context);
validate(_, Context) ->
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new instance with the data provided, if it is valid
%% @end
%%--------------------------------------------------------------------
-spec create/1 :: (#cb_context{}) -> #cb_context{}.
create(#cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"rates">>) of
        {fail, Errors} ->
            crossbar_util:response_invalid_data(Errors, Context);
        {pass, JObj} ->
            {JObj1, _} = add_pvt_fields(JObj, Context),
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
    crossbar_doc:load(Id, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Update an existing instance with the data provided, if it is
%% valid
%% @end
%%--------------------------------------------------------------------
-spec update/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
update(Id, #cb_context{req_data=Data}=Context) ->
    case wh_json_validator:is_valid(Data, <<"rates">>) of
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
%% Check the uploaded file for CSV
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec check_uploaded_file/1 :: (#cb_context{}) -> #cb_context{}.
check_uploaded_file(#cb_context{req_files=[{_Name, File}|_]}=Context) ->
    ?LOG("checking file ~s", [_Name]),
    case wh_json:get_value(<<"contents">>, File) of
        undefined ->
            Context#cb_context{resp_status=error};
        Bin ->
            convert_file(wh_json:get_binary_value([<<"headers">>, <<"content_type">>], File), Bin, Context)
    end;
check_uploaded_file(Context) ->
    ?LOG("no file to process"),
    crossbar_util:response_faulty_request(Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Add any pvt_* fields to the json object
%% @end
%%--------------------------------------------------------------------
add_pvt_fields(JObjs) when is_list(JObjs) ->
    [add_pvt_fields(JObj) || JObj <- JObjs];
add_pvt_fields(JObj) ->
    {JObj1, _} = add_pvt_fields(JObj, undefined),
    JObj1.

add_pvt_fields(JObjs, Context) when is_list(JObjs) ->
    [add_pvt_fields(JObj, Context) || JObj <- JObjs];
add_pvt_fields(JObj, Context) ->
    lists:foldr(fun(F, {J, C}) ->
                        {F(J, C), C}
                end, {JObj, Context}, ?PVT_FUNS).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% These are the pvt funs that add the necessary pvt fields to every
%% instance
%% @end
%%--------------------------------------------------------------------
-spec add_pvt_type/2 :: (wh_json:json_object(), #cb_context{} | 'undefined') -> wh_json:json_object().
add_pvt_type(JObj, _) ->
    wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert the file, based on content-type, to rate documents
%% @end
%%--------------------------------------------------------------------
-spec convert_file/3 :: (nonempty_string(), ne_binary(), #cb_context{}) -> #cb_context{}.
convert_file(<<"text/csv">>, FileContents, Context) ->
    {ok, {Count, Rates}} = csv_to_rates(FileContents),
    case Count of
        0 -> crossbar_util:response(error, <<"no usable rates found">>, 400, [], Context);
        _ -> Context#cb_context{resp_status=success, doc={Count, Rates}}
    end;
convert_file(ContentType, _, Context) ->
    ?LOG("unknown content type: ~s", [ContentType]),
    crossbar_util:response(error, list_to_binary(["unknown content type: ", ContentType]), Context).

-spec csv_to_rates/1 :: (ne_binary()) -> {'ok', {integer(), wh_json:json_objects()}}.
csv_to_rates(CSV) ->
    ecsv:process_csv_binary_with(CSV, fun process_row/2, {0, []}).

-spec process_row/2 :: ([string(),...], {integer(), wh_json:json_objects()}) -> {integer(), wh_json:json_objects()}.
process_row([Prefix, ISO, Desc, InternalCost, Rate], {Cnt, RateDocs}=Acc) ->
    case catch wh_util:to_integer(Prefix) of
        {'EXIT', _} -> Acc;
        _ ->
            Prefix1 = wh_util:to_binary(Prefix),
            ISO1 = strip_quotes(wh_util:to_binary(ISO)),
            Desc1 = strip_quotes(wh_util:to_binary(Desc)),
            InternalCost1 = wh_util:to_binary(InternalCost),
            Rate1 = wh_util:to_binary(Rate),

            %% The idea here is the more expensive rate will have a higher CostF
            %% and decrement it from the weight so it has a lower weight #
            %% meaning it should be more likely used
            CostF = trunc(wh_util:to_float(InternalCost) * 100),

            {Cnt+1, [add_pvt_fields(
                       wh_json:from_list([{<<"_id">>, list_to_binary([ISO1, "-", Prefix])}
                                          ,{<<"prefix">>, Prefix1}
                                          ,{<<"iso_country_code">>, ISO1}
                                          ,{<<"description">>, Desc1}
                                          ,{<<"rate_name">>, Desc1}
                                          ,{<<"rate_cost">>, Rate1}
                                          ,{<<"rate_increment">>, 60}
                                          ,{<<"rate_minimum">>, 60}
                                          ,{<<"rate_surcharge">>, 0}
                                          ,{<<"internal_rate_cost">>, InternalCost1}
                                          ,{<<"weight">>, constrain_weight(byte_size(Prefix1) * 10 - CostF)}

                                          ,{<<"options">>, []}
                                          ,{<<"routes">>, [<<"^\\+", (wh_util:to_binary(Prefix1))/binary, "(\\d*)$">>]}
                                         ]))
                     | RateDocs]}
    end;
process_row(_, Acc) ->
    Acc.

-spec strip_quotes/1 :: (ne_binary()) -> ne_binary().
strip_quotes(Bin) ->
    binary:replace(Bin, [<<"\"">>, <<"\'">>], <<>>, [global]).

-spec constrain_weight/1 :: (integer()) -> 1..100.
constrain_weight(X) when X =< 0 -> 1;
constrain_weight(X) when X >= 100 -> 100;
constrain_weight(X) -> X.
