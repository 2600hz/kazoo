%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, James Aimonetti
%%% @doc
%%% Handle updating devices and emails about voicemails
%%% @end
%%% Created :  3 May 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_vm).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("notify.hrl").
-include_lib("callflow/include/cf_amqp.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).

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
init([]) ->
    {ok, #state{}, 0}.

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
    {reply, ok, State}.

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
handle_info(timeout, _) ->
    start_amqp(),
    {noreply, ok};

handle_info({_, #amqp_msg{props=#'P_basic'{content_type= <<"application/json">>}, payload=Payload}}, State) ->
    logger:format_log(info, "NOTIFY_VM(~p): AMQP Recv ~s~n", [self(), Payload]),
    spawn(fun() ->
		  try
		  JObj = mochijson2:decode(Payload),
		  true = validate(JObj),
		  update_mwi(JObj),
		  send_vm_to_email(JObj)
		  catch A:B -> logger:format_log(info, "NOTIFY_VM: Exception ~p:~p~n~p~n", [A, B, erlang:get_stacktrace()])
		  end
	  end),
    {noreply, State};

handle_info(_Info, State) ->
    logger:format_log(info, "NOTIFY_VM(~p): Unhandled ~p~n", [self(), _Info]),
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

start_amqp() ->
    Q = amqp_util:new_queue(),
    amqp_util:bind_q_to_callevt(Q, ?NOTIFY_VOICEMAIL_NEW, other),
    amqp_util:basic_consume(Q).

validate(JObj) ->
    validate(JObj, wh_json:get_value(<<"Event-Name">>, JObj)).

validate(JObj, <<"new_voicemail">>) ->
    cf_api:new_voicemail_v(JObj);
validate(_, _) ->
    false.

update_mwi(_JObj) ->
    not_implemented_yet.

send_vm_to_email(JObj) ->
    {ok, VMBox} = couch_mgr:open_doc(wh_json:get_value(<<"Account-DB">>, JObj), wh_json:get_value(<<"Voicemail-Box">>, JObj)),
    {ok, UserJObj} = case wh_json:get_value(<<"user_id">>, VMBox) of
			 undefined ->
			     find_user_doc(JObj);
			 Id ->
			     couch_mgr:open_doc(wh_json:get_value(<<"Account-DB">>, JObj), Id)
		     end,
    case wh_json:get_value(<<"email">>, UserJObj) of
	undefined ->
	    logger:format_log(info, "NOTIFY_VM(~p): No email found for user ~p~n", [self(), wh_json:get_value(<<"username">>, UserJObj)]);
	Email ->
	    send_vm_to_email(Email, VMBox, wh_json:get_value(<<"Voicemail-Name">>, JObj))
    end.

send_vm_to_email(Email, VMBox, AttachmentId) ->
    Cmd = whistle_util:to_list(<<(whistle_util:to_binary(code:priv_dir(crossbar)))/binary
                                 ,"/confirmation_email.sh"
                                 ,$ , $", Email/binary, $"
                                 ,$ , $", First/binary, $"
                                 ,$ , $", Last/binary, $"
                                 ,$ , $", BaseURL/binary, $"
                                 ," \"v1/signup/\""
                                 ,$ , $", Key/binary, $"
                               >>),
    os:cmd(Cmd).

%% {"Voicemail-Name":"459c6a174613865ae15c0c5cdcd55967.mp3","Voicemail-Box":"512ee49571d867398ff5b94c8b0fe136","Account-DB":"account%2F4f%2Ffc%2F59464ffb3b43011d08761f258266","To-Realm":"james.thinky64.d-man.org","To-User":"1001","From-Realm":"james.thinky64.d-man.org","From-User":"twinkly","App-Version":"0.7.4","App-Name":"callflow","Event-Name":"new_voicemail","Event-Category":"notification","Server-ID":""}
