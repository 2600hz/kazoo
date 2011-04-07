-module(oauth_client).

-behaviour(gen_server).

-export([access_token_params/1, deauthorize/1, get/2, get/3, get/4, get_access_token/2,
  get_access_token/3, get_access_token/4, get_request_token/2, get_request_token/3,
  get_request_token/4, start/1, start/2, start_link/1, start_link/2, stop/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).

%%============================================================================
%% API functions
%%============================================================================

start(Consumer) ->
  gen_server:start(?MODULE, Consumer, []).

start(ServerName, Consumer) ->
  gen_server:start(ServerName, ?MODULE, Consumer, []).

start_link(Consumer) ->
  gen_server:start_link(?MODULE, Consumer, []).

start_link(ServerName, Consumer) ->
  gen_server:start_link(ServerName, ?MODULE, Consumer, []).

get_request_token(Client, URL) ->
  get_request_token(Client, URL, [], header).

get_request_token(Client, URL, Params) ->
  gen_server:call(Client, {get_request_token, URL, Params, header}).

get_request_token(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {get_request_token, URL, Params, ParamsMethod}).

get_access_token(Client, URL) ->
  get_access_token(Client, URL, [], header).

get_access_token(Client, URL, Params) ->
  gen_server:call(Client, {get_access_token, URL, Params, header}).

get_access_token(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {get_access_token, URL, Params, ParamsMethod}).

get(Client, URL) ->
  get(Client, URL, [], header).

get(Client, URL, Params) ->
  gen_server:call(Client, {get, URL, Params, header}).

get(Client, URL, Params, ParamsMethod) ->
  gen_server:call(Client, {get, URL, Params, ParamsMethod}).

access_token_params(Client) ->
  gen_server:call(Client, {access_token_params}).

deauthorize(Client) ->
  gen_server:cast(Client, deauthorize).

stop(Client) ->
  gen_server:cast(Client, stop).

%%============================================================================
%% Helper functions
%%============================================================================

oauth_get(header, URL, Params, Consumer, Token, TokenSecret) ->
  Signed = oauth:signed_params("GET", URL, Params, Consumer, Token, TokenSecret),
  {AuthorizationParams, QueryParams} = lists:partition(fun({K, _}) -> lists:prefix("oauth_", K) end, Signed),
  Request = {oauth:uri(URL, QueryParams), [oauth:header(AuthorizationParams)]},
  httpc:request(get, Request, [{autoredirect, false}], []);
oauth_get(querystring, URL, Params, Consumer, Token, TokenSecret) ->
  oauth:get(URL, Params, Consumer, Token, TokenSecret).

%%============================================================================
%% gen_server callbacks
%%============================================================================

init(Consumer) ->
  {ok, {Consumer}}.

handle_call({get_request_token, URL, Params, ParamsMethod}, _From, State={Consumer}) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, "", "") of
    {ok, Response} ->
      case oauth_http:response_code(Response) of
        200 ->
          RParams = oauth_http:response_params(Response),
          {reply, {ok, oauth:token(RParams)}, {Consumer, RParams}};
        _ ->
          {reply, Response, State}
      end;
    Error ->
      {reply, Error, State}
  end;
handle_call({get_access_token, URL, Params, ParamsMethod}, _From, State={Consumer, RParams}) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, oauth:token(RParams), oauth:token_secret(RParams)) of
    {ok, Response} ->
      case oauth_http:response_code(Response) of
        200 ->
          AParams = oauth_http:response_params(Response),
          {reply, ok, {Consumer, RParams, AParams}};
        _ ->
          {reply, Response, State}
      end;
    Error ->
      {reply, Error, State}
  end;
handle_call({get, URL, Params, ParamsMethod}, _From, State={Consumer, _RParams, AParams}) ->
  case oauth_get(ParamsMethod, URL, Params, Consumer, oauth:token(AParams), oauth:token_secret(AParams)) of
    {ok, Response={{_, Status, _}, Headers, Body}} ->
      case Status of
        200 ->
          case proplists:get_value("content-type", Headers) of
            undefined ->
              {reply, {ok, Headers, Body}, State};
            ContentType ->
              MediaType = hd(string:tokens(ContentType, ";")),
              case lists:suffix("/xml", MediaType) orelse lists:suffix("+xml", MediaType) of
                true ->
                  {XML, []} = xmerl_scan:string(Body),
                  {reply, {ok, Headers, XML}, State};
                false ->
                  {reply, {ok, Headers, Body}, State}
              end
          end;
        _ ->
          {reply, Response, State}
      end;
    Error ->
      {reply, Error, State}
  end;
handle_call({access_token_params}, _From, State={_Consumer, _RParams, AParams}) ->
  {reply, AParams, State}.

handle_cast(deauthorize, {Consumer, _RParams}) ->
  {noreply, {Consumer}};
handle_cast(deauthorize, {Consumer, _RParams, _AParams}) ->
  {noreply, {Consumer}};
handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info(_Msg, State) ->
  {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(normal, _State) ->
  ok.
