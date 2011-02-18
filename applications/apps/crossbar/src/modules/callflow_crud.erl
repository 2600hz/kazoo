%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow gen server for CRUD
%%%
%%% @end
%%% Created :  3 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
%%%

-module ( callflow_crud ).

-behaviour ( gen_server ).

%% API
-export ( [start_link/0] ).

%% gen_server callbacks
-export ( [init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3] ).

-import ( logger, [format_log/3] ).

-include ( "../crossbar.hrl" ).
-include_lib ( "webmachine/include/webmachine.hrl" ).


-define ( SERVER, ?MODULE ).

-record ( state, { } ).



%%-----------------------------------------------------------------------------
%% PUBLIC API
%%-----------------------------------------------------------------------------
%%

%------------------------------------------------------------------------------
% @public
% @doc
% Starts the server
%
% @end
%------------------------------------------------------------------------------
-spec ( start_link/0 :: ( ) -> tuple(ok, Pid :: pid()) | ignore | tuple(error, Error :: term()) ).
start_link ( ) -> gen_server:start_link( {local, ?SERVER}, ?MODULE, [], [] ).



%%-----------------------------------------------------------------------------
%% GEN SERVER CALLBACKS
%%-----------------------------------------------------------------------------
%%

%------------------------------------------------------------------------------
% @private
% @doc
% Initializes the server
%
% @end
%------------------------------------------------------------------------------
-spec ( init/1 :: (_) ->
     tuple(ok, #state{})
   | tuple(ok, #state{}, Timeout :: integer())
   | ignore
   | tuple(stop, Reason :: term())
).
init ( [] ) -> bind_to_crossbar(), { ok, #state{} }.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles call messages
%
% @end
%------------------------------------------------------------------------------
-spec ( handle_call/3 :: (Request :: term(), From :: term(), State :: term()) -> 
     tuple(reply, Reply :: term(), State :: term())
   | tuple(reply, Reply :: term(), State :: term(), Timeout :: integer())
   | tuple(noreply, State :: term())
   | tuple(noreply, State :: term(), Timeout :: integer())
   | tuple(stop, Reason :: term(), State :: term())
   | tuple(stop, Reason :: term(), Reply :: term(), State :: term())
).
handle_call ( Request, From, State ) ->
   format_log(
      error,
      "CF CRUD (~p): Unhandled call message:~nRequest: ~p~nFrom: ~p~n",
      [self(), Request, From]
   ),
   { reply, ok, State }
.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles cast messages
%
% @end
%------------------------------------------------------------------------------
-spec ( handle_cast/2 :: (Msg :: term(), State :: term()) -> 
     tuple(noreply, State :: term())
   | tuple(noreply, State :: term(), Timeout :: integer())
   | tuple(stop, Reason :: term(), State :: term())
).
handle_cast ( Msg, State ) ->
   format_log(
      error,
      "CF CRUD (~p): Unhandled cast message:~nMessage: ~p~n",
      [self(), Msg]
   ),
   { noreply, State }
.

%------------------------------------------------------------------------------
% @private
% @doc
% Handles all non call/cast messages
%
% @end
%------------------------------------------------------------------------------
-spec ( handle_info/2 :: (Info :: term(), State :: term()) -> 
     tuple(noreply, State :: term())
   | tuple(noreply, State :: term(), Timeout :: integer)
   | tuple(stop, Reason :: term(), State :: term())
).
handle_info ( {binding_fired, Pid, <<"v1_resource.allowed_methods.callflow">>, Payload}, State ) ->
   format_log(info, "CF CRUD (~p): checking allowed methods...", [self()]),
   spawn(fun ( ) ->
      {Result, Payload1} = allowed_methods(Payload),
      Pid ! { binding_result, Result, Payload1 }
   end),
   { noreply, State };
handle_info ( {binding_fired, Pid, <<"v1_resource.resource_exists.callflow">>, Payload}, State ) ->
   format_log(info, "CF CRUD (~p): checking if resource exists...", [self()]),
   spawn(fun ( ) ->
      Pid ! { binding_result, true, Payload }
   end),
   { noreply, State };
handle_info ( {binding_fired, Pid, <<"v1_resource.validate.callflow">>, [RD, Context | Params]}, State ) ->
   format_log(info, "CF CRUD (~p): validating...", [self()]),
   spawn(fun ( ) ->
      Context1 = validate(wrq:method(RD), Params, Context#cb_context{}),
      Pid ! { binding_result, true, [RD, Context1, Params] }
   end),
   { noreply, State };
handle_info ( {binding_fired, Pid, <<"v1_resource.execute.put.callflow">>, [RD, Context | Params]}, State ) ->
   format_log(info, "CF CRUD (~p): putting...", [self()]),
   spawn(fun ( ) ->
      case crossbar_doc:save(Context#cb_context{db_name="callflow"}) of
         #cb_context{resp_status=success}=Context1 -> io:format("Success",[]),
            Pid ! { binding_result, true, [RD, Context1, Params] };
         Else                                      -> io:format("Failure",[]),
            Pid ! { binding_result, true, [RD, Else, Params] }
      end
   end),
   { noreply, State };
handle_info ( {binding_fired, Pid, <<"v1_resource.execute.get.callflow">>, [RD, Context | Params]}, State ) ->
   format_log(info, "CF CRUD (~p): getting...", [self()]),
   case Params of
      [ ] ->
         spawn(fun ( ) ->
            Context1 = crossbar_doc:load_view({"callflow", "list"}, [], Context#cb_context{db_name="callflow"}),
            Pid ! { binding_result, true, [RD, Context#cb_context{resp_data=Context1#cb_context.doc}, Params] }
         end);
      [Id] ->
         spawn(fun ( ) ->
            Context1 = crossbar_doc:load(Id, Context#cb_context{}),
            Pid ! { binding_result, true, [RD, Context1, Params] }
         end)
   end,
   { noreply, State };
handle_info ( {binding_fired, Pid, <<"v1_resource.execute.post.callflow">>, [RD, Context | Params]}, State ) ->
   format_log(info, "CF CRUD (~p): posting...", [self()]),
   spawn(fun ( ) ->
      Context1 = crossbar_doc:save(Context#cb_context{}),
      Pid ! { binding_result, true, [RD, Context1, Params] }
   end),
   { noreply, State };
handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.callflow">>, [RD, Context | Params]}, State) ->
   format_log(info, "CF CRUD (~p): deleting...", [self()]),
   spawn(fun ( ) ->
      Context1 = crossbar_doc:delete(Context#cb_context{}),
      Pid ! { binding_result, true, [RD, Context1, Params] }
   end),
   { noreply, State };
handle_info ( Info, State ) ->
   format_log(
      error,
      "CF CRUD (~p): Unhandled info message:~nInfo: ~p~n",
      [self(), Info]
   ),
   { noreply, State }
.

%------------------------------------------------------------------------------
% @private
% @doc
% Is called by a gen_server when it is about to terminate. It should be the
% opposite of Module:init/1 and do any necessary cleaning up. When it returns,
% the gen_server terminates with Reason. The return value is ignored.
%
% @end
%------------------------------------------------------------------------------
-spec ( terminate/2 :: (Reason :: term(), State :: term()) -> none() ).
terminate ( _Reason, _State ) -> ok.

%------------------------------------------------------------------------------
% @private
% @doc
% Converts process state when code is changed
%
% @end
%------------------------------------------------------------------------------
-spec ( code_change/3 :: (OldVsn :: term(), State :: term(), Extra :: term()) -> tuple(ok, NewState :: term()) ).
code_change ( _OldVsn, State, _Extra ) -> { ok, State }.



%%-----------------------------------------------------------------------------
%% INTERNAL API
%%-----------------------------------------------------------------------------
%%

%------------------------------------------------------------------------------
% @private
% @doc
% binds this server to the crossbar bindings server for the keys we need to
% consume.
%
% @end
%------------------------------------------------------------------------------
-spec ( bind_to_crossbar/0 :: () -> none() ).
bind_to_crossbar ( ) ->
    crossbar_bindings:bind(<<"v1_resource.allowed_methods.callflow">>),
    crossbar_bindings:bind(<<"v1_resource.resource_exists.callflow">>),
    crossbar_bindings:bind(<<"v1_resource.validate.callflow">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.callflow">>)
.

%------------------------------------------------------------------------------
% @private
% @doc
% This function determines the verbs that are appropriate for the given nouns.
%
% @end
%------------------------------------------------------------------------------
-spec ( allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), []) ).
allowed_methods ( [] ) ->
   { true, ['PUT', 'GET'] };                    % PUT - create new callflow
                                                % GET - call flow collection
allowed_methods ( [_] ) ->
   { true, ['PUT', 'GET', 'POST', 'DELETE'] };  % GET    - retrieve callflow
                                                % POST   - update callflow
                                                % DELETE - delete callflow
allowed_methods ( _ ) -> { false, [] }.

%------------------------------------------------------------------------------
% @private
% @doc
% This function determines if the parameters and content are correct for this
% request
%
% @end
%------------------------------------------------------------------------------
-spec ( validate/3 :: (Verb :: atom(), Params :: list(), Context :: #cb_context{}) -> #cb_context{} ).
validate ( 'GET', [], Context ) ->
   format_log(
      progress,
      "CF CRUD (~p): Getting existing callflows:~n",
      [self()]
   ),
   Context#cb_context{resp_status=success};
validate ( 'PUT', [], #cb_context{req_data={struct, Data}}=Context ) ->
   format_log(
      progress,
      "CF CRUD (~p): Creating new callflow:~n~p~n",
      [self(), Data]
   ),
% Data proplist should be checked against a view
   Context#cb_context{doc={struct, Data}, resp_status=success};
validate ( 'GET', [Id], Context ) ->
   format_log(
      progress,
      "CF CRUD (~p): Getting existing callflow by id: ~p~n",
      [self(), Id]
   ),
   crossbar_doc:load(Id, Context);
validate ( 'POST', [Id], #cb_context{req_data={struct, Data}}=Context ) ->
   format_log(
      progress,
      "CF CRUD (~p): Updating existing callflow: ~p~n~p~n",
      [self(), Id, Data]
   ),
% Data proplist should be checked against a view
   crossbar_doc:load_merge(Id, Data, Context);
validate ( 'DELETE', [Id], Context ) ->
   format_log(
      progress,
      "CF CRUD (~p): Deleting existing callflow: ~p~n",
      [self(), Id]
   ),
   crossbar_doc:load(Id, Context);
validate ( _Verb, _Params, Context ) -> crossbar_util:response_faulty_request(Context).

%%%
%%%============================================================================
%%%== END =====
%%%============================================================================
