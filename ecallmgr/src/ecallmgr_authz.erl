%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Make a request for authorization, and answer queries about the CallID
%%% @end
%%% Created :  7 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_authz).

-export([authorize/3, is_authorized/1, default/0, authz_win/1]).

-export([init_authorize/4]).

-include("ecallmgr.hrl").

-define(AUTHZ_LOOP_TIMEOUT, 5000).

%% If authz_default is set to allow, the call is authorized
%% otherwise, the call is not authorized
-spec default/0 :: () -> {boolean(), []}.
default() ->
    case ecallmgr_util:get_setting(authz_default, deny) of
	{ok, allow} -> {true, []};
	_ -> {false, []}
    end.

-spec authorize/3 :: (FSID, CallID, FSData) -> {ok, pid()} when
      FSID :: binary(),
      CallID :: binary(),
      FSData :: proplist().
authorize(FSID, CallID, FSData) ->
    proc_lib:start_link(?MODULE, init_authorize, [self(), FSID, CallID, FSData]).

-spec is_authorized/1 :: (Pid) -> {boolean(), wh_json:json_object()} when
      Pid :: pid() | undefined.
is_authorized(Pid) when is_pid(Pid) ->
    Ref = make_ref(),
    Pid ! {is_authorized, self(), Ref},
    receive
	{is_authorized, Ref, IsAuth, CCV} -> {IsAuth, CCV}
    after
	1000 -> default()
    end;
is_authorized(undefined) -> {false, []}.

-spec authz_win/1 :: (pid() | 'undefined') -> 'ok'.
authz_win(undefined) -> 'ok';
authz_win(Pid) when is_pid(Pid) ->
    Ref = make_ref(),
    Pid ! {authz_win, self(), Ref},
    receive
	{authz_win_sent, Ref} -> ok
    after 1000 ->
	    ?LOG("Timed out sending authz_win, odd")
    end.


-spec init_authorize/4 :: (pid(), ne_binary(), ne_binary(), proplist()) -> no_return().
init_authorize(Parent, FSID, CallID, FSData) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    put(callid, CallID),

    ?LOG_START("Authorize started"),

    ReqProp = request(FSID, CallID, FSData),

    JObj = try
	       {ok, RespJObj} = ecallmgr_amqp_pool:authz_req(ReqProp, 2000),
	       true = wapi_authz:resp_v(RespJObj),
	       RespJObj
	   catch
	       _T:_R ->
		   ?LOG("Authz request un-answered or improper"),
		   ?LOG("Authz ~p: ~p", [_T, _R]),
		   default()
	   end,

    authorize_loop(JObj).

-spec authorize_loop/1 :: (wh_json:json_object()) -> no_return().
authorize_loop(JObj) ->
    receive
	{is_authorized, From, Ref} ->
	    IsAuthz = wh_util:is_true(wh_json:get_value(<<"Is-Authorized">>, JObj)),
	    CCV = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, []),
	    ?LOG("Is authz: ~s", [IsAuthz]),
	    From ! {is_authorized, Ref, IsAuthz, CCV},
	    authorize_loop(JObj);

	{authz_win, From, Ref} ->
	    wapi_authz:publish_win(wh_json:get_value(<<"Server-ID">>, JObj), wh_json:delete_key(<<"Event-Name">>, JObj)),
	    ?LOG("Sent authz_win, nice"),

	    From ! {authz_win_sent, Ref},

	    authorize_loop(JObj);

	_ -> authorize_loop(JObj)
    after ?AUTHZ_LOOP_TIMEOUT ->
	    ?LOG_SYS("Going down from timeout")
    end.

-spec request/3 :: (FSID, CallID, FSData) -> proplist() when
      FSID :: binary(),
      CallID :: binary(),
      FSData :: proplist().
request(FSID, CallID, FSData) ->
    [{<<"Msg-ID">>, FSID}
     ,{<<"Caller-ID-Name">>, props:get_value(<<"Caller-Caller-ID-Name">>, FSData)}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"Caller-Caller-ID-Number">>, FSData)}
     ,{<<"To">>, ecallmgr_util:get_sip_to(FSData)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(FSData)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(FSData)}
     ,{<<"Call-ID">>, CallID}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(FSData))}
     | wh_api:default_headers(<<>>, <<"dialplan">>, <<"authz_req">>, ?APP_NAME, ?APP_VERSION)].
