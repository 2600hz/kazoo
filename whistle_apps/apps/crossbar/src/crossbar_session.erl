%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
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

-define(SERVER, ?MODULE).
-define(MAX_AGE, 1800). % 30 minutes
-define(VIEW_FILE, <<"views/sessions.json">>).
-define(COOKIE_NAME, <<"crossbar_session">>).
-define(SESSION_DB, <<"crossbar%2Fsessions">>).
-define(SESSION_EXPIRED, <<"session/expired_time">>).

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
-spec incr/2 :: (S, K) -> #session{} when
      S :: #session{},
      K :: atom() | binary().
incr(#session{}=S, K) -> incr(S, K, 0).

-spec incr/3 :: (S, K, D) -> #session{} when
      S :: #session{},
      K :: atom() | binary(),
      D :: integer().
incr(#session{storage=Storage}=S, K, D) ->
    case wh_json:get_value(K, Storage) of
        undefined -> store(S, K, D);
        V -> store(S, K, V+1)
    end.

-spec decr/2 :: (S, K) -> #session{} when
      S :: #session{},
      K :: atom() | binary().
decr(#session{}=S, K) -> decr(S, K, 0).

-spec decr/3 :: (S, K, D) -> #session{} when
      S :: #session{},
      K :: atom() | binary(),
      D :: integer().
decr(#session{storage=Storage}=S, K, D) ->
    case wh_json:get_value(K, Storage) of
	undefined -> store(S, K, D);
	V -> store(S, K, V-1)
    end.

%% store(#session(), key(), value()) -> #session().
-spec store/3 :: (S, K, V) -> #session{} when
      S :: #session{},
      K :: atom() | binary(),
      V :: atom() | integer() | binary().
store(#session{storage=Storage}=S, K, V) ->
    S#session{storage=wh_json:set_value(K, V, Storage)}.

-spec retrieve/2 :: (S, K) -> term() when
      S :: #session{},
      K :: atom() | binary().
retrieve(#session{}=S, K) -> retrieve(S, K, undefined).

-spec retrieve/3 :: (S, K, D) -> term() when
      S :: #session{},
      K :: atom() | binary(),
      D :: term().
retrieve(#session{storage=Storage}, K, D) -> wh_json:get_value(K, Storage, D).

%% delete(#session(), key()) -> #session().
-spec delete/2 :: (S, K) -> #session{} when
      S :: #session{},
      K :: atom() | binary().
delete(#session{storage=Storage}=S, K) ->
    S#session{storage=wh_json:delete_key(K, Storage, prune)}.


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
    TRef = erlang:start_timer(60000, self(), clean_expired),
    {ok, TRef}.

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
handle_info({timeout, TRef, clean_expired}, TRef) ->
    spawn(fun() -> clean_expired() end),
    TRef1 = erlang:start_timer(60000, self(), clean_expired),
    {noreply, TRef1};
handle_info(_Info, State) ->
    ?LOG_SYS("Unhandled message: ~p", [_Info]),
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
    ?LOG_SYS("Terminating: ~p", [_Reason]).

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

-spec save_session/2 :: (S, RD) -> #wm_reqdata{} when
      S :: #session{},
      RD :: #wm_reqdata{}.
save_session(#session{created=Created, expires=Expires}=S, RD) ->
    DateTime = calendar:universal_time(),
    Now = calendar:datetime_to_gregorian_seconds(DateTime),
    case has_expired(Created + Expires, Now) of
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

-spec delete_session/2 :: (S, RD) -> #wm_reqdata{} when
      S :: #session{},
      RD :: #wm_reqdata{}.
delete_session(S, RD) ->
    _ = crossbar_bindings:map(<<"session.delete">>, S),
    couch_mgr:del_doc(?SESSION_DB, to_doc(S)),
    set_cookie_header(RD, S, calendar:universal_time(), 0).

-spec to_doc/1 :: (S) -> json_object() when
      S :: #session{}.
to_doc(#session{created=Created}=S) -> to_doc(S, Created).

-spec to_doc/2 :: (S, Now) -> json_object() when
      S :: #session{},
      Now :: integer().
to_doc(#session{'_id'=Id, '_rev'=undefined}=S, Now) ->
    SessionTuple = tuple_to_list(S#session{created=wh_util:to_integer(Now)
					   ,expires=?MAX_AGE
					  }),

    lists:foldr(fun({K,V}, JObj) -> wh_json:set_value(K, V, JObj) end
		,?EMPTY_JSON_OBJECT
		,[{<<"_id">>, Id}
		  ,{<<"session">>, SessionTuple}
		 ]);
to_doc(#session{'_id'=Id, '_rev'=Rev}=S, Now) ->
    SessionTuple = tuple_to_list(S#session{created = wh_util:to_integer(Now)
					   ,expires = ?MAX_AGE
					  }),

    lists:foldr(fun({K,V}, JObj) -> wh_json:set_value(K, V, JObj) end
		,?EMPTY_JSON_OBJECT
		,[{<<"_id">>, Id}
		  ,{<<"_rev">>, Rev}
		  ,{<<"session">>, SessionTuple}
		 ]).

-spec from_doc/1 :: (JObj) -> #session{} when
      JObj :: json_object().
from_doc(JObj) ->
    Id = wh_json:get_value(<<"_id">>, JObj),
    Rev = wh_json:get_value(<<"_rev">>, JObj),
    #session{created=Created, expires=Expires}=S = setelement(1, list_to_tuple(wh_json:get_value(<<"session">>, JObj)), session),
    S#session{'_id'=Id, '_rev'=Rev
	      ,created=wh_util:to_integer(Created)
	      ,expires=wh_util:to_integer(Expires)
	     }.

%% create a new session record
-spec new/1 :: (Id) -> #session{} when
      Id :: binary().
new(Id) ->
    Now = wh_util:current_tstamp(),
    #session{created=Now, expires=?MAX_AGE, '_id'=Id}.

-spec set_cookie_header/4 :: (RD, Session, DateTime, MaxAge) -> #wm_reqdata{} when
      RD :: #wm_reqdata{},
      Session :: #session{},
      DateTime :: wh_datetime(),
      MaxAge :: 0 | ?MAX_AGE.
set_cookie_header(RD, #session{'_id'=Id}, DateTime, MaxAge) ->
    {CookieHeader, CookieValue} =
	mochiweb_cookies:cookie(?COOKIE_NAME, Id, [{max_age, MaxAge}, {local_time, DateTime}]),
    wrq:set_resp_header(CookieHeader, CookieValue, RD).

%% Maintenance Functions

%% Retrieve expired sessions and delete them from the DB
-spec clean_expired/0 :: () -> ok.
clean_expired() ->
    case couch_mgr:get_results(?SESSION_DB, ?SESSION_EXPIRED, [{<<"startkey">>, 0},
							       {<<"endkey">>, wh_util:current_tstamp()}
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
	    ?LOG_SYS("Cleaned ~b sessions", [length(Docs)]),
	    lists:foreach(fun(D) -> couch_mgr:del_doc(?SESSION_DB, D) end, Docs)
    end.

clean_expired_({struct, _}) -> true;
clean_expired_(_) -> false.

%% pass the secs representation of the expires time and compare to now
-spec has_expired/1 :: (E) -> boolean() when
      E :: integer().
has_expired(E) ->
    has_expired(E, wh_util:current_tstamp()).

-spec has_expired/2 :: (E, Now) -> boolean() when
      E :: integer(),
      Now :: integer().
has_expired(E, Now) when E < Now -> true;
has_expired(_, _) -> false.
