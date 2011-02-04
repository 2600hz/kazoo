%%%----------------------------------------------------------------------------
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%%
%%% @end
%%% Created :  11 Jan 2011 by Vladimir Darmin <vova@2600hz.org>
%%%----------------------------------------------------------------------------

-module ( callflow ).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include ( "../crossbar.hrl" ).
-include_lib ( "webmachine/include/webmachine.hrl" ).


-define(SERVER, ?MODULE).

-record(state, {}).



%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
%%

%%-----------------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%-----------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).



%%-----------------------------------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------------------------------
%%

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
init([]) -> bind_to_crossbar(), {ok, #state{}}.

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
handle_call(_Request, _From, State) -> Reply = ok, {reply, Reply, State}.

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
handle_cast(_Msg, State) -> io:format("Unhandled ~p", [_Msg]), {noreply, State}.

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
handle_info({binding_fired, Pid, <<"v1_resource.allowed_methods.callflow">>, Payload}, State) ->
   spawn(fun ( ) ->
      {Result, Payload1} = allowed_methods(Payload),
      Pid ! {binding_result, Result, Payload1}
   end),
   {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.resource_exists.callflow">>, Payload}, State) ->
   spawn(fun ( ) ->
      Pid ! {binding_result, true, Payload}
   end),
   {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.validate.callflow">>, [RD, Context | Params]}, State) ->
   spawn(fun ( ) ->
      Context1 = validate(wrq:method(RD), Params, Context#cb_context{db_name="callflow"}),
      Pid ! {binding_result, true, [RD, Context1, Params]}
%      Pid ! {binding_result, true, [RD, Context#cb_context{resp_status=success} , Params]}
   end),
   {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.execute.put.callflow">>, [RD, Context | Params]}, State) ->
   spawn(fun ( ) ->
      case crossbar_doc:save(Context#cb_context{db_name="callflow"}) of
         #cb_context{resp_status=success}=Context1 ->
            Pid ! {binding_result, true, [RD, Context1, Params]};
         Else                                      ->
            Pid ! {binding_result, true, [RD, Else, Params]}
      end
   end),
   {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.execute.get.callflow">>, [RD, Context | Params]}, State) ->
   case Params of
      [ ] ->
         spawn(fun ( ) ->
            Pid ! {binding_result, true, [RD, Context#cb_context{resp_data=[<<"list of existing callflows">>]}, Params]}
         end);
      [Id] ->
         spawn(fun ( ) ->
            Context1 = crossbar_doc:load(Id, Context#cb_context{db_name="callflow"}),
            Pid ! {binding_result, true, [RD, Context1, Params]}
         end)
   end,
   {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.execute.post.callflow">>, [RD, Context | Params]}, State) ->
   spawn(fun ( ) ->
      Context1 = crossbar_doc:save(Context#cb_context{db_name="callflow"}),
      Pid ! {binding_result, true, [RD, Context1, Params]}
   end),
   {noreply, State};
handle_info({binding_fired, Pid, <<"v1_resource.execute.delete.callflow">>, [RD, Context | Params]}, State) ->
   spawn(fun ( ) ->
      Context1 = crossbar_doc:delete(Context#cb_context{db_name="callflow"}),
      Pid ! {binding_result, true, [RD, Context1, Params]}
   end),
   {noreply, State};
handle_info({binding_fired, Pid, _, Payload}, State) ->
   spawn(fun ( ) ->
      Pid ! {binding_result, true, Payload}
   end),
   {noreply, State}
.

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
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.



%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
%%

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% This function binds this server to the crossbar bindings server,
%% for the keys we need to consume.
%% @end
%%-----------------------------------------------------------------------------
-spec(bind_to_crossbar/0 :: () -> no_return()).
bind_to_crossbar() ->
    crossbar_bindings:bind(<<"v1_resource.allowed_methods.callflow">>),
    crossbar_bindings:bind(<<"v1_resource.resource_exists.callflow">>),
    crossbar_bindings:bind(<<"v1_resource.validate.callflow">>),
    crossbar_bindings:bind(<<"v1_resource.execute.#.callflow">>)
.




%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% This function determines the verbs that are appropriate for the given nouns.
%%
%% @end
%%-----------------------------------------------------------------------------
-spec ( allowed_methods/1 :: (Paths :: list()) -> tuple(boolean(), []) ).
allowed_methods( [] ) ->
   { true, ['PUT', 'GET'] };                    % PUT - create new callflow
                                                % GET - call flow collection
allowed_methods( [_] ) ->
   { true, ['PUT', 'GET', 'POST', 'DELETE'] };  % GET    - retrieve callflow
                                                % POST   - update callflow
                                                % DELETE - delete callflow
allowed_methods( _ ) -> { false, [] }.




%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec(validate/3 :: (Verb :: atom(), Params :: list(), Context :: #cb_context{}) -> #cb_context{}).
validate('GET', [], Context) -> io:format("~p~n", ["getting list of callflows"]), Context;
validate('PUT', [], #cb_context{req_data={struct,Data}}=Context) ->io:format("~p~n", ["creating callflow"]),
   Doc = Data,
   Context#cb_context{doc={struct, Doc}, resp_status=success}
;
validate('GET', [DocId], Context) ->
   crossbar_doc:load(DocId, Context);
validate('POST', [DocId], #cb_context{req_data={struct,Data}}=Context) ->
   crossbar_doc:load_merge(DocId, Data, Context);
validate('DELETE', [DocId], Context) ->
   crossbar_doc:load(DocId, Context);
validate(_, _, Context) -> crossbar_util:response_faulty_request(Context).

























%%-----------------------------------------------------------------------------
%% Proplist helpers
%%-----------------------------------------------------------------------------
%%


%%-----------------------------------------------------------------------------
%% Conversion functions
%%-----------------------------------------------------------------------------
%%


%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% This function converts proplist, array, or simple term into true json
%%
%% @end
%%-----------------------------------------------------------------------------
%-spec ( json/1 :: (Data :: { proplist() } | list() | term()) -> list() ).
%json ( {} )           -> "{}";
%json ( {Properties} ) -> "{"++ prop_to_json(Properties) ++"}";
%json ( [] )           -> "[]";
%json ( [Elements] )   -> "["++ array_to_json([Elements]) ++"]";
%json ( Term )         -> term_to_string(Term).

%prop_to_json ( {P , V} ) -> term_to_string(P) ++":"++ json(V);
%prop_to_json ( [] ) -> "";
%prop_to_json ( [E | R] ) ->
%   if
%      length(R) > 0 -> prop_to_json(E) ++","++ prop_to_json(R);
%      true          -> prop_to_json(E)
%   end
%.

%array_to_json ( [] ) -> "";
%array_to_json ( [E | R] ) ->
%   if
%      length(R) > 0 -> json(E) ++","++ array_to_json(R);
%      true          -> json(E)
%   end
%.

%term_to_string ( T ) ->
%   if
%      is_binary(T)    -> String = binary_to_list(T);
%      is_atom(T)      -> String = atom_to_list(T);
%      is_bitstring(T) -> String = bitstring_to_list(T);
%      is_integer(T)   -> String = integer_to_list(T);
%      is_float(T)     -> String = float_to_list(T);
%      true            -> String = "[Object]"
%   end,
%   "\""++String++"\""
%.

%------------------------------------------------------------------------------
%  This function helps converting the list returned by mochijson2:encode
%  into proper json format (getting rid of << >>)
%
%json_struct_to_string ( [] )      -> "";
%json_struct_to_string ( [E | R] ) -> json_struct_to_string(E) ++ json_struct_to_string(R);
%json_struct_to_string ( E )       ->
%   if
%      is_binary(E) -> String = binary_to_list(E);
%      is_atom(E)   -> String = atom_to_list(E);
%      is_list(E)   -> String = E;
%      true         -> String = [ E ]
%   end,
%   String
%.
%------------------------------------------------------------------------------


%%
%%-----------------------------------------------------------------------------
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  THE END  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%-----------------------------------------------------------------------------
