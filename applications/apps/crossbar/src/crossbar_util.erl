%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% 
%%% @end
%%% Created : 14 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_util).

-export([response/2, response/3, response/4, response/5]).
-export([response_faulty_request/1]).
-export([response_bad_identifier/2]).
-export([response_datastore_timeout/1]).
-export([response_datastore_conn_refused/1]).
-export([response_invalid_data/2]).
-export([response_missing_view/1]).
-export([response_db_missing/1]).
-export([response_db_fatal/1]).
-export([binding_heartbeat/1]).


-export([get_request_params/1, param_exists/2, find_failed/2]).
-export([winkstart_envelope/1]).
-export([winkstart_envelope/2, winkstart_envelope/3, winkstart_envelope/4]).

-export([is_valid_request_envelope/1, is_valid_response_envelope/1]).

-include("crossbar.hrl").

-import(logger, [format_log/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function set the response status to success, and load the provided
%% data.
%% @end
%%--------------------------------------------------------------------
-spec(response/2 :: (Data :: term(), Context :: #cb_context{}) -> #cb_context{}).
response(Data, Context) ->
    create_response(success, undefined, undefined, Data, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function load the error message into a 500 response, of type
%% fatal or error.
%% @end
%%--------------------------------------------------------------------
-spec(response/3 :: (Status :: error|fatal, Msg :: json_string(), Context :: #cb_context{}) -> #cb_context{}).
response(error, Msg, Context) ->
    create_response(error, Msg, 500, [], Context);
response(fatal, Msg, Context) ->
    create_response(fatal, Msg, 500, [], Context).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function load the error message into a specifc code response,
%% of type fatal or error.
%% @end
%%--------------------------------------------------------------------
-spec(response/4 :: (Status :: error|fatal, Msg :: json_string(), Code :: integer()|undefined, Context :: #cb_context{}) -> #cb_context{}).
response(error, Msg, Code, Context) ->
    create_response(error, Msg, Code, [], Context);
response(fatal, Msg, Code, Context) ->
    create_response(fatal, Msg, Code, [], Context).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function load the error message into a specifc code response,
%% of type fatal or error with additional data
%% @end
%%--------------------------------------------------------------------
-spec(response/5 :: (Status :: error|fatal, Msg :: json_string(), Code :: integer()|undefined, Data :: mochijson(), Context :: #cb_context{}) -> #cb_context{}).
response(error, Msg, Code, Data, Context) ->
    create_response(error, Msg, Code, Data, Context);
response(fatal, Msg, Code, Data, Context) ->
    create_response(fatal, Msg, Code, Data, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function loads the response vars in Context, soon it will
%% make smarter chooices about formating resp_data and filtering
%% other parameters.
%% @end
%%--------------------------------------------------------------------
-spec(create_response/5 :: (Status :: error|fatal|success, Msg :: json_string(), Code :: integer()|undefined, Data :: mochijson(), Context :: #cb_context{}) -> #cb_context{}).
create_response(Status, Msg, Code, Data, Context) ->
    Context#cb_context {
         resp_status = Status
        ,resp_error_msg = Msg
        ,resp_error_code = Code
        ,resp_data = Data
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the request is faulty (doesnt have a
%% match in validation, or someother issue with it keeps it from being
%% processed, like nonsensical chains)
%% @end
%%--------------------------------------------------------------------
-spec(response_faulty_request/1 :: (Context :: #cb_context{}) -> #cb_context{}).
response_faulty_request(Context) ->
    response(error, <<"faulty request">>, 400, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested ID did not match a
%% data record
%% @end
%%--------------------------------------------------------------------
-spec(response_bad_identifier/2 :: (Id :: binary(), Context :: #cb_context{}) -> #cb_context{}).
response_bad_identifier(Id, Context) ->
    response(error, <<"bad identifier">>, 410, [Id], Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested data query was missing
%% @end
%%--------------------------------------------------------------------
-spec(response_missing_view/1 :: (Context :: #cb_context{}) -> #cb_context{}).
response_missing_view(Context) ->
    response(fatal, <<"datastore missing view">>, 500, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore time'd out
%% @end
%%--------------------------------------------------------------------
-spec(response_datastore_timeout/1 :: (Context :: #cb_context{}) -> #cb_context{}).
response_datastore_timeout(Context) ->
    response(error, <<"datastore timeout">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore time'd out
%% @end
%%--------------------------------------------------------------------
-spec(response_datastore_conn_refused/1 :: (Context :: #cb_context{}) -> #cb_context{}).
response_datastore_conn_refused(Context) ->
    response(error, <<"datastore connection refused">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the provided data did not validate
%% @end
%%--------------------------------------------------------------------
-spec(response_invalid_data/2 :: (Fields :: list(), Context :: #cb_context{}) -> #cb_context{}).
response_invalid_data(Fields, Context) ->
    response(error, <<"invalid data">>, 400, Fields, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore does not have the requested
%% record collection
%% @end
%%--------------------------------------------------------------------
-spec(response_db_missing/1 :: (Context :: #cb_context{}) -> #cb_context{}).
response_db_missing(Context) ->
    response(fatal, <<"data collection missing">>, 503, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore does not have the requested
%% record collection
%% @end
%%--------------------------------------------------------------------
-spec(response_db_fatal/1 :: (Context :: #cb_context{}) -> #cb_context{}).
response_db_fatal(Context) ->
    response(fatal, <<"datastore fatal error">>, 500, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This spawns a function that will monitor the parent and hearbeat
%% the crossbar_binding PID provided as long as the parent lives
%% @end
%%--------------------------------------------------------------------
-spec(binding_heartbeat/1 :: (BPid :: pid()) -> pid()).
binding_heartbeat(BPid) ->
    PPid=self(),
    spawn(fun() ->
        erlang:monitor(process, PPid),
        {ok, Tref} = timer:send_interval(250, BPid, heartbeat),
        receive _ -> ok after 10000 -> ok end,
        timer:cancel(Tref)
    end).

%% If using a fun to validate, it should return a boolean
-spec(param_exists/2 :: (Params :: proplist()
			 , tuple(Key :: binary(), function() | proplist())
			  | tuple(Key :: binary(), function() | proplist(), optional)
			) -> tuple(binary(), boolean() | proplist())).
param_exists(Params, {Key, Fun}) when is_function(Fun) ->
    case props:get_value(Key, Params) of
	undefined -> {Key, false};
	Val ->
	    try
		{Key, Fun(Val)}
	    catch
		_:_ -> {Key, false}
	    end
    end;
param_exists(Params, {Key, NestedParams}) ->
    case props:get_value(Key, Params) of
	undefined -> {Key, false};
	{struct, SubParams} ->
	    {Key, lists:map(fun(R) -> param_exists(SubParams, R) end, NestedParams)};
	_ ->
	    case NestedParams of
		[] -> {Key, true};
		[_|_] -> {Key, false}
	    end
    end;
param_exists(Params, {Key, Fun, optional}) when is_function(Fun) ->
    case props:get_value(Key, Params) of
	undefined -> {Key, true};
	Val ->
	    try
		{Key, Fun(Val)}
	    catch
		_:_ -> {Key, false}
	    end
    end;
param_exists(Params, {Key, NestedParams, optional}) ->
    case props:get_value(Key, Params) of
	undefined -> {Key, true};
	{struct, SubParams} ->
	    {Key, lists:map(fun(R) -> param_exists(SubParams, R) end, NestedParams)};
	_ ->
	    case NestedParams of
		[] -> {Key, true};
		[_|_] -> {Key, false}
	    end
    end.

%% searches for any keys that have failed
-spec(find_failed/2 :: (tuple(binary(), boolean() | proplist()), list(binary())) -> list(binary())).
find_failed({_, true}, Acc) -> Acc;
find_failed({K, false}, Acc) -> [K | Acc];
find_failed({K, L}, Acc) when is_list(L) ->
    case lists:foldl(fun find_failed/2, [], L) of
	[] -> Acc; % no nested K/V pairs failed
	L1 -> [[K | L1] | Acc] %% if one or more nested failed, Key failed
    end.

get_request_params(RD) ->
    case wrq:method(RD) of
	'GET' -> wrq:req_qs(RD);
	_ -> pull_from_body_and_qs(RD)
    end.

%% Favor body paramaters when key exists in both body and qs
pull_from_body_and_qs(RD) ->
    ReqBody = wrq:req_body(RD),
    PostBody = try
		   {struct, Prop} = mochijson2:decode(ReqBody),
		   Prop
	       catch
		   _:_ -> mochiweb_util:parse_qs(ReqBody)
	       end,
    QS = wrq:req_qs(RD),
    lists:ukeymerge(1, lists:ukeysort(1, PostBody), lists:ukeysort(1, QS)).

-spec(is_valid_request_envelope/1 :: (JSON :: json_object()) -> boolean()).
is_valid_request_envelope(JSON) ->
    whapps_json:get_value([<<"data">>], JSON, not_found) =/= not_found.

-spec(is_valid_response_envelope/1 :: (JSON :: json_object()) -> boolean()).
is_valid_response_envelope(JSON) ->
     undefined =/= whapps_json:get_value(["data"], JSON) andalso
	undefined =/= whapps_json:get_value(["status"], JSON).

-spec(winkstart_envelope/1 :: (ApiResult :: crossbar_module_result()) -> iolist()).
winkstart_envelope({Status, Data}) -> winkstart_envelope(Status, Data);    
winkstart_envelope({Status, Data, Msg}) -> winkstart_envelope(Status, Data, Msg);
winkstart_envelope({Status, Data, Msg, Code}) -> winkstart_envelope(Status, Data, Msg, Code).

-spec(winkstart_envelope/2 :: (Status :: crossbar_status(), Data :: proplist()) -> iolist()).
winkstart_envelope(success, Data) ->
    format_log(info, "Envelope: D: ~p~n", [Data]),
    mochijson2:encode({struct, [{status, <<"success">>}
				,{data, {struct, Data}}
			       ]});
winkstart_envelope(error, Data) ->
    winkstart_envelope(error, Data, "An unspecified error has occurred");
winkstart_envelope(fatal, Data) ->
    winkstart_envelope(error, Data, "An unspecified fatal error has occurred").

-spec(winkstart_envelope/3 :: (Status :: crossbar_status(), Data :: proplist(), Msg :: string()) -> iolist()).
winkstart_envelope(success, Data, Msg) ->
    mochijson2:encode({struct, [{status, <<"success">>}
				,{message, whistle_util:to_binary(Msg)}
				,{data, {struct, Data}}
			       ]});
winkstart_envelope(error, Data, Msg) ->
    mochijson2:encode({struct, [{status, <<"error">>}
				,{message, whistle_util:to_binary(Msg)}
				,{data, {struct, Data}}
			       ]});
winkstart_envelope(fatal, Data, Msg) ->
    mochijson2:encode({struct, [{status, <<"fatal">>}
				,{message, whistle_util:to_binary(Msg)}
				,{data, {struct, Data}}
			       ]}).

-spec(winkstart_envelope/4 :: (Status :: crossbar_status(), Data :: proplist(), ErrorMsg :: string(), ErrorCode :: integer()) -> iolist()).
winkstart_envelope(success, Data, Msg, _) ->
    winkstart_envelope(success, Data, Msg);
winkstart_envelope(error, Data, ErrorMsg, ErrorCode) ->
    mochijson2:encode({struct, [{status, <<"error">>}
				,{error, ErrorCode}
				,{message, whistle_util:to_binary(ErrorMsg)}
				,{data, {struct, Data}}
			       ]});
winkstart_envelope(fatal, Data, ErrorMsg, ErrorCode) ->
    mochijson2:encode({struct, [{status, <<"fatal">>}
				,{error, ErrorCode}
				,{message, whistle_util:to_binary(ErrorMsg)}
				,{data, {struct, Data}}
			       ]}).

%% EUNIT TESTING
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

-endif.
