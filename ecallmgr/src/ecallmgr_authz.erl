%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Make a request for authorization, and answer queries about the CallID
%%% @end
%%% Created :  7 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_authz).

-export([authorize/3, is_authorized/1, default/0]).

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

-spec is_authorized/1 :: (Pid) -> {boolean(), json_object()} when
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

-spec init_authorize/4 :: (Parent, FSID, CallID, FSData) -> no_return() when
      Parent :: pid(),
      FSID :: binary(),
      CallID :: binary(),
      FSData :: proplist().
init_authorize(Parent, FSID, CallID, FSData) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    put(callid, CallID),

    ?LOG_START("Authorize started"),

    ReqProp = request(FSID, CallID, FSData),

    {IsAuth, CCV} = try
			{ok, RespJObj} = ecallmgr_amqp_pool:authz_req(ReqProp, 2000),
			{wh_util:is_true(wh_json:get_value(<<"Is-Authorized">>, RespJObj))
			 ,wh_json:get_value(<<"Custom-Channel-Vars">>, RespJObj, [])}
		    catch
			_:_ ->
			    ?LOG("Authz request un-answered"),
			    default()
		    end,

    authorize_loop(IsAuth, CCV).

-spec authorize_loop/2 :: (IsAuth, CCV) -> no_return() when
      IsAuth :: boolean(),
      CCV :: json_object().
authorize_loop(IsAuth, CCV) ->
    ?LOG("Is Authorized: ~s", [IsAuth]),
    receive
	{is_authorized, From, Ref} ->
	    From ! {is_authorized, Ref, IsAuth, CCV};
	_ -> authorize_loop(IsAuth, CCV)
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
     ,{<<"Custom-Channel-Vars">>, {struct, ecallmgr_util:custom_channel_vars(FSData)}}
     | wh_api:default_headers(<<>>, <<"dialplan">>, <<"authz_req">>, ?APP_NAME, ?APP_VERSION)].
