%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011 VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 14 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_util).

-export([response/2, response/3, response/4, response/5]).
-export([response_deprecated/1, response_deprecated_redirect/2, response_deprecated_redirect/3]).
-export([response_faulty_request/1]).
-export([response_bad_identifier/2]).
-export([response_conflicting_docs/1]).
-export([response_datastore_timeout/1]).
-export([response_datastore_conn_refused/1]).
-export([response_invalid_data/2]).
-export([response_missing_view/1]).
-export([response_db_missing/1]).
-export([response_db_fatal/1]).
-export([binding_heartbeat/1, binding_heartbeat/2]).
-export([put_reqid/1]).
-export([store/3, fetch/2, get_abs_url/2]).

-include("../include/crossbar.hrl").
-include_lib("webmachine/include/webmachine.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function set the response status to success, and load the provided
%% data.
%% @end
%%--------------------------------------------------------------------
-spec(response/2 :: (JTerm :: json_term(), Context :: #cb_context{}) -> #cb_context{}).
response(JTerm, Context) ->
    create_response(success, undefined, undefined, JTerm, Context).

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
-spec(response/5 :: (Status :: error|fatal, Msg :: json_string(), Code :: integer()|undefined, JTerm :: json_term(), Context :: #cb_context{}) -> #cb_context{}).
response(error, Msg, Code, JTerm, Context) ->
    create_response(error, Msg, Code, JTerm, Context);
response(fatal, Msg, Code, JTerm, Context) ->
    create_response(fatal, Msg, Code, JTerm, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function loads the response vars in Context, soon it will
%% make smarter chooices about formating resp_data and filtering
%% other parameters.
%% @end
%%--------------------------------------------------------------------
-spec create_response/5 :: (error|fatal|success, json_string(), integer()|undefined, json_term(), #cb_context{}) -> #cb_context{}.
create_response(Status, Msg, Code, JTerm, Context) ->
    Context#cb_context {
         resp_status = Status
        ,resp_error_msg = Msg
        ,resp_error_code = Code
        ,resp_data = JTerm
    }.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the request is faulty (doesnt have a
%% match in validation, or someother issue with it keeps it from being
%% processed, like nonsensical chains)
%% @end
%%--------------------------------------------------------------------
-spec response_faulty_request/1 :: (#cb_context{}) -> #cb_context{}.
response_faulty_request(Context) ->
    response(error, <<"faulty request">>, 400, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% When a module is no longer valid, alert the client of the deprecated status
%% by either sending a 410 Gone or a 301 Redirct (when using the arity
%% 3 version.
%%
%% The RedirectUrl should be relative to the accessed URL. So, if the
%% URL accessed that is deprecated is:
%% /v1/account/{AID}/module/{MID}
%% and that MID moved to module2, the RedirectURL should be:
%% ../../module2/{MID}
%%
%% If redirecting from module1 to module2, RedirectURL should be:
%% ../module2
%% @end
%%--------------------------------------------------------------------
-spec response_deprecated(#cb_context{}) -> #cb_context{}.
response_deprecated(Context) ->
    create_response(error, <<"deprecated">>, 410, wh_json:new(), Context).

-spec response_deprecated_redirect(#cb_context{}, json_string()) -> #cb_context{}.
-spec response_deprecated_redirect(#cb_context{}, json_string(), json_object()) -> #cb_context{}.
response_deprecated_redirect(Context, RedirectUrl) ->
    response_deprecated_redirect(Context, RedirectUrl, wh_json:new()).
response_deprecated_redirect(#cb_context{resp_headers=RespHeaders}=Context, RedirectUrl, JObj) ->
    create_response(error, <<"deprecated">>, 301, JObj
		    ,Context#cb_context{resp_headers=[{"Location", RedirectUrl} | RespHeaders]}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested ID did not match a
%% data record. Using 404 as 410 is a permanent Gone, while 404 is
%% a softer not found now.
%% @end
%%--------------------------------------------------------------------
-spec(response_bad_identifier/2 :: (Id :: binary(), Context :: #cb_context{}) -> #cb_context{}).
response_bad_identifier(Id, Context) ->
    response(error, <<"bad identifier">>, 404, [Id], Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the requested resource update fails
%% because of a conflict in the DB
%% @end
%%--------------------------------------------------------------------
-spec(response_conflicting_docs/1 :: (Context :: #cb_context{}) -> #cb_context{}).
response_conflicting_docs(Context) ->
    response(error, <<"conflicting documents">>, 409, Context).

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
-spec(response_invalid_data/2 :: (JTerm :: json_term(), Context :: #cb_context{}) -> #cb_context{}).
response_invalid_data(JTerm, Context) ->
    response(error, <<"invalid data">>, 400, JTerm, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Create a standard response if the datastore does not have the requested
%% record collection
%% @end
%%--------------------------------------------------------------------
-spec(response_db_missing/1 :: (Context :: #cb_context{}) -> #cb_context{}).
response_db_missing(Context) ->
    response(fatal, <<"data collection missing: database not found">>, 503, Context).

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
-spec binding_heartbeat/1 :: (BPid) -> pid() when
      BPid :: pid().
binding_heartbeat(BPid) ->
    binding_heartbeat(BPid, 10000).

-spec binding_heartbeat/2 :: (BPid, Timeout) -> pid() when
      BPid :: pid(),
      Timeout :: non_neg_integer() | 'infinity'.
binding_heartbeat(BPid, Timeout) ->
    PPid = self(),
    ?LOG("Starting binding heartbeat for ~p", [PPid]),
    spawn(fun() ->
		  Ref = erlang:monitor(process, PPid),
		  {ok, Tref} = timer:send_interval(250, BPid, heartbeat),
		  ok = receive
			   {'DOWN', Ref, process, _, normal} ->
			       ok;
			   {'DOWN', Ref, process, _, Reason} ->
			       ?LOG("Bound client (~p) down for non-normal reason: ~p", [PPid, Reason]),
			       BPid ! {binding_error, Reason};
			   _ -> ok
		       after Timeout ->
			       ?LOG("Bound client (~p) too slow, timed out after ~p", [PPid, Timeout]),
			       ok
		       end,
		  timer:cancel(Tref)
	  end).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function extracts the request ID and sets it as 'callid' in
%% the process dictionary, where the logger expects it.
%% @end
%%--------------------------------------------------------------------
-spec put_reqid/1 :: (Context) -> no_return() when
      Context :: #cb_context{}.
put_reqid(#cb_context{req_id=ReqId}) ->
    put(callid, ReqId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets a value in the crossbar context for later retrieval during
%% this request.
%% @end
%%--------------------------------------------------------------------
-spec store/3 :: (Key, Data, Context) -> #cb_context{} when
      Key :: term(),
      Data :: term(),
      Context :: #cb_context{}.
store(Key, Data, #cb_context{storage=Storage}=Context) ->
    Context#cb_context{storage=[{Key, Data}|proplists:delete(Key, Storage)]}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Fetches a previously stored value from the current request.
%% @end
%%--------------------------------------------------------------------
-spec fetch/2 :: (Key, Context) -> term() when
      Key :: term(),
      Context :: #cb_context{}.
fetch(Key, #cb_context{storage=Storage}) ->
    proplists:get_value(Key, Storage).

-spec get_abs_url/2 :: (#wm_reqdata{}, ne_binary()) -> string().
get_abs_url(RD, Url) ->
    %% http://some.host.com:port/"
    Port = case wrq:port(RD) of
	       80 -> "";
	       P -> [":", wh_util:to_list(P)]
	   end,

    Host = ["http://", string:join(lists:reverse(wrq:host_tokens(RD)), "."), Port, "/"],
    ?LOG("host: ~s", [Host]),

    PathTokensRev = lists:reverse(string:tokens(wrq:path(RD), "/")),
    get_abs_url(Host, PathTokensRev, Url).

%% Request: http[s]://some.host.com:port/v1/accounts/acct_id/module
%%
%% Host : http[s]://some.host.com:port/
%% PathTokensRev: /v1/accounts/acct_id/module => [module, acct_id, accounts, v1]
%% Url: ../other_mod
%%
%% Result: http[s]://some.host.com:port/v1/accounts/acct_id/other_mod
-spec get_abs_url/3 :: (iolist(), [ne_binary(),...], ne_binary() | string()) -> string().
get_abs_url(Host, PathTokensRev, Url) ->
    UrlTokens = string:tokens(wh_util:to_list(Url), "/"),

    ?LOG("path: ~p", [PathTokensRev]),
    ?LOG("rel: ~p", [UrlTokens]),

    Url1 = string:join(
      lists:reverse(
	lists:foldl(fun("..", []) -> [];
		       ("..", [_ | PathTokens]) -> PathTokens;
		       (".", PathTokens) -> PathTokens;
		       (Segment, PathTokens) -> [Segment | PathTokens]
		    end, PathTokensRev, UrlTokens)
       ), "/"),
    ?LOG("final url: ~p", [Url1]),
    binary_to_list(list_to_binary([Host, Url1])).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_abs_url_test() ->
    Host = ["http://", "some.host.com", ":8000", "/"],
    PTs = ["module", "acct_id", "accounts", "v1"],
    Url = "../other_mod",
    ?assertEqual(get_abs_url(Host, PTs, Url), "http://some.host.com:8000/v1/accounts/acct_id/other_mod"),
    ?assertEqual(get_abs_url(Host, ["mod_id" | PTs], "../../other_mod"++"/mod_id"), "http://some.host.com:8000/v1/accounts/acct_id/other_mod/mod_id").

-endif.
