%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Session server
%%% @end
%%% Created :  9 Dec 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(crossbar_session).

-behaviour(gen_server).

%% API
-export([start_link/0, is_authorized/1, start_session/1, finish_session/2, end_session/2]).

%% Access functions to the session storage
-export([incr/2, incr/3, decr/2, decr/3, store/3, retrieve/2, retrieve/3, delete/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-define(SERVER, ?MODULE).
-define(MAX_AGE, 1800). % 30 minutes
-define(VIEW_FILE, "views/sessions.json").
-define(COOKIE_NAME, "crossbar_session").
-define(SESSION_DB, "crossbar%2Fsessions").
-define(SESSION_EXPIRED, {"session", "expired_time"}).

-include_lib("webmachine/include/webmachine.hrl").
-include("../include/crossbar.hrl").

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

%% Begin or resume a session, returning the session record
-spec(start_session/1 :: (AuthToken :: undefined | binary()) -> #session{}).
start_session(AuthToken) ->
    gen_server:call(?MODULE, {start_session, AuthToken}, infinity).

%% return the modified request datastructure with the updated session
-spec(finish_session/2 :: (S :: #session{}, RD :: #wm_reqdata{}) -> #wm_reqdata{}).
finish_session(#session{}=S, #wm_reqdata{}=RD) ->
    gen_server:call(?MODULE, {finish_session, S, RD}, infinity).

%% close the session and delete the cookie from the request datastructure
-spec(end_session/2 :: (S :: #session{}, RD :: #wm_reqdata{}) -> #wm_reqdata{}).
end_session(#session{}=S, #wm_reqdata{}=RD) ->
    gen_server:call(?MODULE, {stop_session, S, RD}, infinity).

-spec(is_authorized/1 :: (S :: #session{}) -> boolean()).
is_authorized(#session{}=S) ->
    gen_server:call(?MODULE, {is_authorized, S}, infinity).

%% increment the field's value
-spec(incr/2 :: (S :: #session{}, K :: term()) -> #session{}).
incr(#session{}=S, K) -> incr(S, K, 0).

-spec(incr/3 :: (S :: #session{}, K :: term(), D :: integer()) -> #session{}).
incr(#session{storage=Storage}=S, K, D) ->
    case props:get_value(K, Storage) of
        undefined -> store(S, K, D);
        V -> store(S, K, V+1)
    end.

-spec(decr/2 :: (S :: #session{}, K :: term()) -> #session{}).
decr(#session{}=S, K) -> decr(S, K, 0).
-spec(decr/3 :: (S :: #session{}, K :: term(), D :: integer()) -> #session{}).
decr(#session{storage=Storage}=S, K, D) ->
    case props:get_value(K, Storage) of
	undefined -> store(S, K, D);
	V -> store(S, K, V-1)
    end.

%% store(#session(), key(), value()) -> #session().
store(#session{storage=Storage}=S, K, V) ->
    S#session{storage=[{K, V} | lists:keydelete(K, 1, Storage)]}.

%% retrieve(#session(), key()) -> undefined | term()
retrieve(#session{}=S, K) -> retrieve(S, K, undefined).
%% retrieve(#session(), key()) -> D | term()
retrieve(#session{storage=Storage}, K, D) -> props:get_value(K, Storage, D).

%% delete(#session(), key()) -> #session().
delete(#session{storage=Storage}=S, K) ->
    S#session{storage=lists:keydelete(K, 1, Storage)}.


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
init([]) ->
    couch_mgr:db_create(?SESSION_DB),
    couch_mgr:load_doc_from_file(?SESSION_DB, crossbar, ?VIEW_FILE),
    {ok, _} = timer:send_interval(60000, ?MODULE, clean_expired),
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
handle_call({start_session, AuthToken}, _, State) ->
    {reply, get_session(AuthToken), State};

%% return the modified request datastructure with the updated session
handle_call({finish_session, #session{}=S, #wm_reqdata{}=RD}, _, State) ->
    {reply, save_session(S, RD), State};

%% close the session and delete the cookie from the request datastructure
handle_call({stop_session, #session{}=S, #wm_reqdata{}=RD}, _, State) ->
    {reply, delete_session(S, RD), State};

handle_call({is_authorized, #session{account_id=AccountId, created=Created, expires=Expires}}, _, State) ->
    {reply, AccountId =/= undefined andalso (not has_expired(Created + Expires)), State}.

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
handle_info(clean_expired, State) ->
    spawn(fun() -> clean_expired() end),
    {noreply, State};
handle_info(_Info, State) ->
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

%% get_session_doc(request()) -> not_found | json_object()
-spec(get_session_doc/1 :: (AuthToken :: binary()) -> not_found | json_object()).
get_session_doc(AuthToken) ->
    case couch_mgr:open_doc(?SESSION_DB, AuthToken) of
	{error, _} -> not_found;
	{ok, JObj} -> JObj
    end.

%% get_session_rec(request()) -> session_rec()
-spec(get_session/1 :: (AuthToken :: binary()) -> undefined | #session{}).
get_session(<<>>) ->
    undefined;
get_session(AuthToken) ->
    case get_session_doc(AuthToken) of
        not_found -> new(AuthToken);
        JObj ->
            #session{created=Created, expires=Expires}=S = from_doc(JObj),
	    case has_expired(Created+Expires) of
		true -> new(AuthToken);
		false -> S
	    end
    end.

-spec(save_session/2 :: (S :: #session{}, RD :: #wm_reqdata{}) -> #wm_reqdata{}).
save_session(#session{created=Created, expires=Expires}=S, RD) ->
    DateTime = calendar:universal_time(),
    Now = calendar:datetime_to_gregorian_seconds(DateTime),
    case has_expired(Created + Expires) of
        true ->
            delete_session(S, RD);
        false ->
            case couch_mgr:save_doc(?SESSION_DB, to_doc(S, Now)) of
                {error, _Error} ->
                    delete_session(S, RD);
                {ok, JObj} ->
                    set_cookie_header(RD, from_doc(JObj), DateTime, ?MAX_AGE)
            end
    end.

-spec(delete_session/2 :: (S :: #session{}, RD :: #wm_reqdata{}) -> #wm_reqdata{}).
delete_session(S, RD) ->
    _ = crossbar_bindings:map(<<"session.delete">>, S),
    couch_mgr:del_doc(?SESSION_DB, to_doc(S)),
    set_cookie_header(RD, S, calendar:local_time(), -1).

-spec(to_doc/1 :: (S :: #session{}) -> json_object()).
to_doc(#session{created=Created}=S) -> to_doc(S, Created).

-spec(to_doc/2 :: (S :: #session{}, Now :: integer()) -> json_object()).
to_doc(#session{'_id'=Id, '_rev'=undefined, storage=Storage}=S, Now) ->
    {struct, [{<<"_id">>, Id}
             ,{<<"session">>, tuple_to_list(S#session{created=whistle_util:to_integer(Now)
					      ,expires=?MAX_AGE
					      ,storage=encode_storage(Storage)
					     })}
    ]};
to_doc(#session{'_id'=Id, '_rev'=Rev, storage=Storage}=S, Now) ->
    {struct, [{<<"_id">>, Id}
	      ,{<<"_rev">>, Rev}
	      ,{<<"session">>, tuple_to_list(S#session{created= whistle_util:to_integer(Now)
						       ,expires=?MAX_AGE
						       ,storage=encode_storage(Storage)
						      })}
	     ]}.

-spec(encode_storage/1 :: (L :: proplist()) -> [] | json_object()).
encode_storage([]) -> [];
encode_storage(L) when is_list(L) -> {struct, L}.

-spec(from_doc/1 :: (Doc :: json_object()) -> #session{}).
from_doc({struct, Doc}) ->
    Id = props:get_value(<<"_id">>, Doc),
    Rev = props:get_value(<<"_rev">>, Doc),
    #session{created=Created, expires=Expires}=S = setelement(1, list_to_tuple(props:get_value(<<"session">>, Doc)), session),
    S#session{'_id'=Id, '_rev'=Rev
	      ,created=whistle_util:to_integer(Created)
	      ,expires=whistle_util:to_integer(Expires)
	     }.

%% create a new session record
%% new() -> #session()
-spec(new/1 :: (Id :: binary()) -> #session{}).
new(Id) ->
    Now = current_seconds(),
    #session{created=Now, expires=?MAX_AGE, '_id'=Id}.

%% set_cookie_header(request(), #session(), {date(), time()}, seconds()) -> request()
-spec(set_cookie_header/4 :: (RD :: #wm_reqdata{}, Session :: #session{}, DateTime :: tuple(), MaxAge :: integer()) -> #wm_reqdata{}).
set_cookie_header(RD, #session{'_id'=Id}, DateTime, MaxAge) ->
    {CookieHeader, CookieValue} = mochiweb_cookies:cookie(?COOKIE_NAME, Id, [{max_age, MaxAge},
									     {local_time, DateTime}
									    ]),
    wrq:set_resp_header(CookieHeader, CookieValue, RD).

%% Maintenance Functions

%% Retrieve expired sessions and delete them from the DB
-spec(clean_expired/0 :: () -> no_return()).
clean_expired() ->
    case couch_mgr:get_results(?SESSION_DB, ?SESSION_EXPIRED, [{"startkey", 0},
							       {"endkey", current_seconds()}
							      ]) of
	{error, _} -> ok;
	{ok, Sessions} ->
	    Docs = [ D || D <- [ begin
				     case couch_mgr:open_doc(?SESSION_DB, props:get_value(<<"id">>, Prop)) of
					 {ok, Doc} -> Doc;
					 Else -> Else
				     end
				 end || {struct, Prop} <- Sessions ]
			      ,clean_expired_(D)],
	    format_log(info, "CB_SESSION(~p): Cleaned ~p sessions~n", [self(), length(Docs)]),
	    lists:foreach(fun(D) -> couch_mgr:del_doc(?SESSION_DB, D) end, Docs)
    end.

clean_expired_({struct, _}) -> true;
clean_expired_(_) -> false.

%% pass the secs representation of the expires time and compare to now
-spec(has_expired/1 :: (E :: integer()) -> boolean()).
has_expired(E) ->
    E < current_seconds().

-spec(current_seconds/0 :: () -> integer()).
current_seconds() ->
    whistle_util:current_tstamp().
