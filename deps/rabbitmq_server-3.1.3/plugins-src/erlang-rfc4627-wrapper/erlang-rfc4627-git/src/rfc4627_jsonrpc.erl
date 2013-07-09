%% JSON-RPC, transport-neutral.
%%---------------------------------------------------------------------------
%% @author Tony Garnock-Jones <tonygarnockjones@gmail.com>
%% @author LShift Ltd. <query@lshift.net>
%% @copyright 2007-2010, 2011, 2012 Tony Garnock-Jones and 2007-2010 LShift Ltd.
%% @license
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------
%% @since 1.2.0
%%
%% @reference the <a href="http://json-rpc.org/wd/JSON-RPC-1-1-WD-20060807.html">JSON-RPC specification</a> (draft; <a href="JSON-RPC-1-1-WD-20060807.html">mirrored locally</a>)
%%
%% @doc Provides a registry of running JSON-RPC objects, and a
%% transport-neutral means of invoking methods defined on such
%% objects.
%%
%% Other modules provide interfaces to specific transports and
%% transport implementations. See {@link rfc4627_jsonrpc_http} and
%% {@link rfc4627_jsonrpc_inets}, for example.
%%
%% == JSON-RPC Objects are Erlang Processes ==
%%
%% In the normal case, each JSON-RPC object in a running system
%% corresponds to one Erlang process. This makes object lifecycle
%% control very natural.
%%
%% == Basic usage ==
%%
%% Ensure the registry process is running, using {@link start/0} or
%% {@link start_link/0}. Once it's up and running, use {@link
%% register_service/2} to expose a process as a JSON-RPC service.
%%
%% To register a service, you will need to describe the methods
%% available on it. Use {@link service/4} to do so.
%%
%% == Implementing a service ==
%%
%% Your service should be implemented by a `gen_server'
%% process. JSON-RPC requests will be sent to it as
%% `gen_server:call/2' messages:
%%
%% ``{jsonrpc, ProcedureNameBin, RequestInfo, Args}''
%% 
%% Your module's `handle_call' function should respond to these
%% messages with a reply of type {@type jsonrpc_response()}.
%%
%% Here's the implementation of the "test_proc" example:
%%
%% ```
%% handle_call({jsonrpc, <<"test_proc">>, _RequestInfo, [Value]}, _From, State) ->
%%     {reply, {result, <<"ErlangServer: ", Value/binary>>}, State}.
%% '''
%%
%% See also the complete example Erlang module included with the
%% source code, `test_jsonrpc_inets.erl'.
%%
%% == Registering a service with the Service Registry ==
%%
%% You will need to ``-include("rfc4627_jsonrpc.hrl").'' (Or, if
%% you've installed the compiled `rfc4627_jsonrpc' code in your Erlang
%% lib directory,
%% ``-include_lib("rfc4627/include/rfc4627_jsonrpc.hrl").'')
%%
%% The service registry must be started before any registrations can
%% be performed: simply call {@link start/0} or {@link
%% start_link/0}. This will start the registry if it wasn't running,
%% or if it was, it will inform you of the existing registry's Pid.
%%
%% Registering a service is as simple as starting a process to receive
%% service requests, and passing its pid to `rfc4627_jsonrpc' along
%% with a <a
%% href="JSON-RPC-1-1-WD-20060807.html#ServiceDescription">service
%% descriptor</a> object built from Erlang records defined in
%% `mod_jsonrpc.hrl':
%%
%% ```
%% {ok, Pid} = gen_server:start(?MODULE, [], []),
%% rfc4627_jsonrpc:register_service
%%       (Pid,
%%        rfc4627_jsonrpc:service(<<"test">>,
%%                                <<"urn:uuid:afe1b4b5-23b0-4964-a74a-9168535c96b2">>,
%%                                <<"1.0">>,
%%                                [#service_proc{name = <<"test_proc">>,
%%                                               idempotent = true,
%%                                               params = [#service_proc_param{name = <<"value">>,
%%                                                                             type = <<"str">>}]}])).
%% '''
%%
%% This code registers a service called "test":
%%
%% <ul>
%%  <li>its name is "test"</li>
%%  <li>its identifier (JSON-RPC's service description "id" field) is "urn:uuid:afe1b4b5-23b0-4964-a74a-9168535c96b2"</li>
%%  <li>its version string is "1.0"</li>
%%  <li>
%%	it defines just one method/procedure, which
%%	<ul>
%%	  <li>is named "test_proc"</li>
%%	  <li>is marked "idempotent", which means it is permitted to be accessed via HTTP GET instead of only HTTP POST</li>
%%	  <li>has a single parameter named "value" of type "str"</li>
%%	</ul>
%%  </li>
%% </ul>
%%
%% Note that almost all of the string values are expressed as
%% binaries: this is because {@link rfc4627} uses binaries to
%% represent JSON strings.
%%
%% To register a service with multiple procedures, add additional
%% `#service_proc' records to the procedure list in the call to {@link
%% service/4}. Similarly, additional parameters for each procedure can
%% be defined by the addition of extra `#service_proc_param' records
%% in the appropriate place.
%%
%% The available types for parameters are the strings defined in <a
%% href="JSON-RPC-1-1-WD-20060807.html#ParameterReturnTypeStrings">this
%% part</a> of the JSON-RPC specification, namely "bit", "num", "str",
%% "arr", "obj", "any" or "nil". See also
%% `rfc4627_jsonrpc:proc_param_type/1'.
%%
%% == Invoking methods on local services ==
%%
%% Usually, JSON-RPC services are invoked via HTTP using {@link
%% rfc4627_jsonrpc_inets} or similar. However, the interface used by
%% specific network transports to call methods on services is also
%% available to ordinary programs. (And, of course, programs that
%% implement new kinds of network transport for JSON-RPC.)
%%
%% To invoke a local service method, first retrieve its service
%% descriptor using {@link lookup_service/1}. Then use {@link
%% jsonrpc_post/3} or {@link invoke_service_method/8} to call a method
%% on the service.
%%
%% The service record as retrieved from the registry contains the pid
%% of the process responsible for handling service requests.
%%
%% == Experimental extension: 'Stateless' services ==
%%
%% Instead of registering a pid with the `rfc4627_jsonrpc'
%% registry, an alternative is to use a service record with a
%% function object instead of a pid. This allows more control over
%% how a service is implemented: if using a `gen_server'
%% service is too heavy, a function object that sends a simple
%% message could be used; or if the service didn't need an
%% implementing process at all, the function object could process
%% the request without sending any messages at all.
%%
%% To build a service descriptor object with a function handler
%% instead of a pid, call `rfc4627_jsonrpc:service/5'
%% instead of `rfc4627_jsonrpc:service/4':
%%
%% ```
%% rfc4627_jsonrpc:service({function, fun my_handler/3}, Name, Id, Version, Procs)
%%     -> service descriptor object
%%
%% my_handler(ProcedureNameBin, RequestInfo, Args) -> jsonrpc_response()
%% '''
%%
%% The resulting service descriptor can be registered with {@link
%% register_service/1} as well as used directly with {@link
%% invoke_service_method/8}.
%%
%% @type service() = #service{}. A service description record, as
%% defined in `rfc4627_jsonrpc.hrl'. Can be constructed using {@link
%% service/4}, or retrieved from the registry using {@link
%% lookup_service/1}.
%%
%% @type json() = rfc4627:json(). A JSON value.
%% @type jsonobj() = rfc4627:jsonobj(). A JSON "object" or "struct".
%% @type jsonarray() = rfc4627:jsonarray(). A JSON array.
%%
%% @type jsonrpc_response() = {(result | error), json()} | {(result | error), json(), jsonobj()}.
%%
%% The type that JSON-RPC service implementations are required to
%% return.
%%
%% The first value should be `result' for a normal return value, or
%% `error' for an error response. Use {@link error_response/2} or
%% {@link error_response/3} to construct approprate error response
%% values.
%%
%% The second value is the main response body: for normal returns,
%% this is the ordinary return value of the procedure, and for error
%% responses, it is the response structure defined in the JSON-RPC
%% specification.
%%
%% The third, optional, value is called `ResponseInfo'. It can be used
%% by the service implementation to supply transport-specific
%% information back to its caller. For instance, if invoked via HTTP,
%% extra headers to send back to the HTTP client can be passed in the
%% `ResponseInfo' object. If `ResponseInfo' is omitted, `{obj, []}' is
%% assumed.

-module(rfc4627_jsonrpc).
-include("rfc4627.hrl").
-include("rfc4627_jsonrpc.hrl").

-export([start/0, start_link/0]).

-export([lookup_service/1, register_service/1, register_service/2]).
-export([gen_object_name/0, system_describe/2]).
-export([jsonrpc_post/3, jsonrpc_post/4, invoke_service_method/8, expand_jsonrpc_reply/2]).
-export([error_response/2, error_response/3, service/4, service/5, proc/2]).

-define(SERVICE, ?MODULE).

%% @spec () -> {ok, pid()} | {error, {already_started, pid()}}
%% @doc Starts the registry process.
start() ->
    gen_server:start({local, ?SERVICE}, rfc4627_jsonrpc_registry, [], []).

%% @spec () -> {ok, pid()} | {error, {already_started, pid()}}
%% @doc Starts the registry process, linking it to the calling process.
start_link() ->
    gen_server:start_link({local, ?SERVICE}, rfc4627_jsonrpc_registry, [], []).

%% @spec (binary()) -> not_found | service()
%% @doc Calls the registry to look up a service by name.
lookup_service(Service) ->
    gen_server:call(?SERVICE, {lookup_service, Service}).

%% @spec (service()) -> ok
%% @doc Registers a JSON-RPC service.
%%
%% The name of the service is contained within its service record.
register_service(ServiceDescription) ->
    gen_server:call(?SERVICE, {register_service, ServiceDescription}).

%% @spec (pid(), service()) -> ok
%% @doc Registers a JSON-RPC service.
%%
%% The name of the service is contained within its service record.
register_service(Pid, ServiceDescription) ->
    gen_server:call(?SERVICE, {register_service, Pid, ServiceDescription}).

%% @spec () -> string()
%% @doc Generates a unique name that can be used for otherwise unnamed JSON-RPC services.
gen_object_name() ->
    Hash = erlang:md5(term_to_binary({node(), erlang:now()})),
    binary_to_hex(Hash).

%% @spec (binary(), Service::service()) -> jsonobj()
%% @doc Builds a JSON-RPC service description JSON object.
%%
%% This is used in the implementation of the `system.describe'
%% JSON-RPC method that all JSON-RPC services are required by the
%% specification to support.
%%
%% If `EndpointAddress' is `undefined', no `address' field is returned
%% in the resulting description. Otherwise, it is included
%% verbatim. The other fields in the description are constructed using
%% the information in the `Service' record.
system_describe(EndpointAddress,
		#service{name = Name, id = Id, version = Version, summary = Summary,
			 help = Help, procs = Procs}) ->
    remove_undefined({obj, [{"sdversion", <<"1.0">>},
			    {"name", Name},
			    {"id", Id},
			    {"version", Version},
			    {"summary", Summary},
			    {"help", Help},
			    {"address", EndpointAddress},
			    {"procs", [system_describe_proc(P) || P <- Procs]}]}).

%% @spec (service(), jsonobj(), jsonobj()) -> jsonrpc_response()
%% @doc Calls {@link jsonrpc_post/4} with a `Timeout' of `default'.
jsonrpc_post(ServiceRec, RequestInfo, RequestObj) ->
    jsonrpc_post(ServiceRec, RequestInfo, RequestObj, default).

%% @spec (service(), jsonobj(), jsonobj(), Timeout) -> jsonrpc_response()
%% where Timeout = default | infinity | integer()
%%
%% @doc Performs a POST-style invocation of a JSON-RPC service method.
%%
%% `RequestObj' is to be a JSON "object" containing at minimum fields
%% named "id", "method" and "params", with meanings as defined by the
%% JSON-RPC specification.
%%
%% See {@link invoke_service_method/8} for descriptions of the other
%% parameters.
jsonrpc_post(ServiceRec, RequestInfo, RequestObj, Timeout) ->
    Id = rfc4627:get_field(RequestObj, "id", undefined),
    Method = rfc4627:get_field(RequestObj, "method", undefined),
    Args = rfc4627:get_field(RequestObj, "params", undefined),
    invoke_service_method(ServiceRec, Id, post, RequestInfo, undefined, Method, Args, Timeout).

%% @spec (ServiceRec, RequestId, PostOrGet, RequestInfo, EndpointAddress, Method, Args, Timeout)
%%       -> jsonrpc_response()
%% where ServiceRec = service()
%%       RequestId = integer() | null
%%       PostOrGet = post | get
%%       RequestInfo = jsonobj()
%%       EndpointAddress = binary() | undefined
%%       Method = binary()
%%       Args = jsonobj() | jsonarray()
%%       Timeout = default | infinity | integer()
%%
%% @doc Calls a method defined on a JSON-RPC service.
%%
%% Use {@link lookup_service/1} or {@link service/5} to get a usable
%% #service record for use with this function.
%%
%% The request ID should be the ID from the JSON-RPC request, as it
%% was encoded for the transport the request arrived on. It will be
%% used by this function in constructing the JSON-RPC reply
%% object. Since the request ID is optional, it is acceptable to
%% supply `null' instead of an integer.
%%
%% The `PostOrGet' parameter is used to check the idempotency setting
%% for the chosen service procedure. If the parameter is passed as
%% `post', no check is performed, as it is assumed that a stateful
%% method call is permitted; if it is passed as `get', then the
%% idempotency flag is checked, and an error object may be returned in
%% the case that the invoked method is non-idempotent.
%%
%% The `RequestInfo' structure contains transport-specific details
%% about the request. For HTTP, for example, this will include the
%% HTTP headers and HTTP method. For AMQP, it will include the
%% exchange and routing-key.
%%
%% The `EndpointAddress' is only used in the case that the method
%% being invoked is `system.describe', in which case it is
%% incorporated into the returned description as detailed in the
%% documentation for {@link system_describe/2}.
%%
%% `Method' is the name of the service method to invoke, and `Args' is
%% either a JSON "object" or a JSON array, to serve as the parameters
%% for the call.
%%
%% The `Timeout' parameter is used to control how long the system will
%% wait for a reply from the backing gen_server. If `default' is
%% specified, the default `gen_server:call' timeout is used;
%% otherwise, `infinity' or a number of milliseconds is passed in to
%% the `gen_server:call'.
invoke_service_method(ServiceRec = #service{}, RequestId,
		      PostOrGet, RequestInfo, EndpointAddress, Method, Args, Timeout) ->
    expand_jsonrpc_reply(
      RequestId,
      case Method of
	  <<"system.describe">> ->
	      {result, system_describe(EndpointAddress, ServiceRec)};
	  <<"system.", _Rest/binary>> ->
	      error_response(403, "System methods forbidden", Method);
	  _ ->
	      case lookup_service_proc(ServiceRec, Method) of
		  {ok, ServiceProc} ->
		      invoke_service(PostOrGet, ServiceRec#service.handler,
				     RequestInfo, ServiceProc, Args, Timeout);
		  not_found ->
		      error_response(404, "Procedure not found", [EndpointAddress, Method])
	      end
      end).

%% @spec (CodeOrMessage::(integer() | string() | binary()), json()) -> {error, jsonobj()}
%% @doc Constructs an error response as per the JSON-RPC specification.
%%
%% Either a code or a message can be supplied as the first argument.
%%
%% @see error_response/3
error_response(Code, ErrorValue) when is_integer(Code) ->
    error_response(Code, "Error "++integer_to_list(Code), ErrorValue);
error_response(Message, ErrorValue) when is_list(Message) ->
    error_response(500, list_to_binary(Message), ErrorValue);
error_response(Message, ErrorValue) when is_binary(Message) ->
    error_response(500, Message, ErrorValue).

%% @spec (integer(), (string() | binary()), json()) -> {error, jsonobj()}
%% @doc Constructs an error response as per the JSON-RPC specification.
%%
%% The first argument should hold the error code. Error codes are
%% defined in the JSON-RPC specification.
%%
%% The second argument can be either a `string()' or a `binary()'
%% describing the error as text.
%%
%% The third argument is a general JSON value providing arbitrary
%% further detail on the error.
error_response(Code, Message, ErrorValue) when is_list(Message) ->
    error_response(Code, list_to_binary(Message), ErrorValue);
error_response(Code, Message, ErrorValue) ->
    {error, {obj, [{"name", <<"JSONRPCError">>},
		   {"code", Code},
		   {"message", Message},
		   {"error", ErrorValue}]}}.

%% @spec (Name, Id, Version, Procs) -> service()
%% where Name = binary() | string()
%%       Id = binary() | string()
%%       Version = binary() | string()
%%       Procs = [ProcedureDescription]
%%       ProcedureDescription = {Name, [Parameter]} | #service_proc{}
%%       Parameter = {Name, ParameterType} | #service_proc_param{}
%%       ParameterType = bit | num | str | arr | obj | any | nil | string() | binary()
%%
%% @doc Constructs a service description record.
%%
%% The `Procs' parameter should be a list of procedure-descriptions,
%% which can be either constructed manually or using {@link proc/2}.
service(Name, Id, Version, Procs) when is_list(Name) ->
    service(list_to_binary(Name), Id, Version, Procs);
service(Name, Id, Version, Procs) when is_list(Id) ->
    service(Name, list_to_binary(Id), Version, Procs);
service(Name, Id, Version, Procs) when is_list(Version) ->
    service(Name, Id, list_to_binary(Version), Procs);
service(Name, Id, Version, Procs) ->
    #service{name = Name, id = Id, version = Version,
	     procs = [case P of
			  {ProcName, Params} -> proc(ProcName, Params);
			  #service_proc{} -> P
		      end || P <- Procs]}.

%% @spec (Handler, Name, Id, Version, Procs) -> service()
%% where Handler = {pid, pid()} | {function, function()}
%%
%% @doc As for {@link service/4}, but supplying a handler for use with
%% an experimental "stateless" service implementation.
service(Handler, Name, Id, Version, Procs) ->
    (service(Name, Id, Version, Procs))#service{handler = Handler}.

%% @spec (Name, [Parameter]) -> #service_proc{}
%% where Name = binary() | string()
%%       Parameter = {Name, ParameterType} | #service_proc_param{}
%%       ParameterType = bit | num | str | arr | obj | any | nil | string() | binary()
%%
%% @doc Constructs a service procedure description record.
proc(Name, Params) when is_list(Name) ->
    proc(list_to_binary(Name), Params);
proc(Name, Params) ->
    #service_proc{name = Name, params = [proc_param(P) || P <- Params]}.

%---------------------------------------------------------------------------

build_jsonrpc_response(Id, ResultField) ->
    {obj, [{version, <<"1.1">>},
	   {id, Id},
	   ResultField]}.

expand_jsonrpc_reply(RequestId, {ResultOrError, Value}) ->
    {ResultOrError, build_jsonrpc_response(RequestId, {ResultOrError, Value}), {obj, []}};
expand_jsonrpc_reply(RequestId, {ResultOrError, Value, ResponseInfo}) ->
    {ResultOrError, build_jsonrpc_response(RequestId, {ResultOrError, Value}), ResponseInfo}.

proc_param({N, T}) when is_list(N) ->
    proc_param({list_to_binary(N), T});
proc_param({N, T}) ->
    #service_proc_param{name = N, type = proc_param_type(T)}.

proc_param_type(bit) -> <<"bit">>;
proc_param_type(num) -> <<"num">>;
proc_param_type(str) -> <<"str">>;
proc_param_type(arr) -> <<"arr">>;
proc_param_type(obj) -> <<"obj">>;
proc_param_type(any) -> <<"any">>;
proc_param_type(nil) -> <<"nil">>;
proc_param_type(T) when is_list(T) -> list_to_binary(T);
proc_param_type(T) when is_binary(T) -> T.

binary_to_hex(<<>>) ->
    [];
binary_to_hex(<<B, Rest/binary>>) ->
    [rfc4627:hex_digit((B bsr 4) band 15),
     rfc4627:hex_digit(B band 15) |
     binary_to_hex(Rest)].

lookup_service_proc(#service{procs = Procs}, Method) ->
    case lists:keysearch(Method, #service_proc.name, Procs) of
	{value, ServiceProc} ->
	    {ok, ServiceProc};
	false ->
	    not_found
    end.

invoke_service(get, Handler, RequestInfo, ServiceProc, Args, Timeout) ->
    if
	ServiceProc#service_proc.idempotent ->
	    invoke_service1(Handler, RequestInfo, ServiceProc, Args, Timeout);
	true ->
	    error_response(403, "Non-idempotent method", ServiceProc#service_proc.name)
    end;
invoke_service(post, Handler, RequestInfo, ServiceProc, Args, Timeout) ->
    invoke_service1(Handler, RequestInfo, ServiceProc, Args, Timeout).

invoke_service1(Handler, RequestInfo, #service_proc{name = Name, params = Params}, Args, Timeout) ->
    case catch run_handler(Handler, Name, RequestInfo, coerce_args(Params, Args), Timeout) of
	{'EXIT', {{function_clause, _}, _}} ->
	    error_response(404, "Undefined procedure", Name);
	{'EXIT', Reason} ->
	    error_response(500, "Internal error", list_to_binary(io_lib:format("~p", [Reason])));
	Response ->
	    Response
    end.

run_handler({pid, Pid}, Name, RequestInfo, CoercedArgs, default) ->
    gen_server:call(Pid, {jsonrpc, Name, RequestInfo, CoercedArgs});
run_handler({pid, Pid}, Name, RequestInfo, CoercedArgs, Timeout) ->
    gen_server:call(Pid, {jsonrpc, Name, RequestInfo, CoercedArgs}, Timeout);
run_handler({function, F}, Name, RequestInfo, CoercedArgs, _Timeout) ->
    F(Name, RequestInfo, CoercedArgs).

coerce_args(_Params, Args) when is_list(Args) ->
    Args;
coerce_args(Params, {obj, Fields}) ->
    [case lists:keysearch(binary_to_list(Name), 1, Fields) of
	 {value, {_, Value}} -> coerce_value(Value, Type);
	 false -> null
     end || #service_proc_param{name = Name, type = Type} <- Params].

coerce_value(Value, _Type) when not(is_binary(Value)) ->
    Value;
coerce_value(<<"true">>, <<"bit">>) -> true;
coerce_value(_, <<"bit">>) -> false;
coerce_value(V, <<"num">>) -> list_to_integer(binary_to_list(V));
coerce_value(V, <<"str">>) -> V;
coerce_value(V, <<"arr">>) -> rfc4627:decode(V);
coerce_value(V, <<"obj">>) -> rfc4627:decode(V);
coerce_value(V, <<"any">>) -> V;
coerce_value(_, <<"nil">>) -> null;
coerce_value(V, _) -> V.

remove_undefined({obj, Fields}) ->
    {obj, remove_undefined1(Fields)}.

remove_undefined1([]) ->
    [];
remove_undefined1([{_, undefined} | Rest]) ->
    remove_undefined1(Rest);
remove_undefined1([X | Rest]) ->
    [X | remove_undefined1(Rest)].

system_describe_proc(P = #service_proc{params = Params}) ->
    remove_undefined(?RFC4627_FROM_RECORD(service_proc,
					  P#service_proc{params = [system_describe_proc_param(A)
								   || A <- Params]})).

system_describe_proc_param(P = #service_proc_param{}) ->
    remove_undefined(?RFC4627_FROM_RECORD(service_proc_param, P)).
