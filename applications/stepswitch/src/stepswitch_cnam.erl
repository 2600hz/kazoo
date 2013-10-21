%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%% Lookup cnam
%%% @end
%%%-------------------------------------------------------------------
-module(stepswitch_cnam).

-behaviour(gen_server).

-export([start_link/1]).
-export([render/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,handle_event/2
         ,terminate/2
         ,code_change/3
        ]).

-export([lookup/1]).

-include("stepswitch.hrl").

-define(CONFIG_CAT, <<"stepswitch.cnam">>).

-define(DEFAULT_EXPIRES, 900).
-define(DEFAULT_METHOD, <<"get">>).
-define(DEFAULT_CONTENT, <<>>).
-define(DEFAULT_URL, <<"https://api.opencnam.com/v2/phone/{{phone_number}}">>).
-define(DEFAULT_ACCEPT_HDR, <<"text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>).
-define(DEFAULT_USER_AGENT_HDR, <<"Kazoo Stepswitch CNAM">>).
-define(DEFAULT_CONTENT_TYPE_HDR, <<"application/json">>).


-define(HTTP_ACCEPT_HEADER, whapps_config:get_string(?CONFIG_CAT, <<"http_accept_header">>, ?DEFAULT_ACCEPT_HDR)).
-define(HTTP_USER_AGENT, whapps_config:get_string(?CONFIG_CAT, <<"http_user_agent_header">>, ?DEFAULT_USER_AGENT_HDR)).
-define(HTTP_CONTENT_TYPE, whapps_config:get_string(?CONFIG_CAT, <<"http_content_type_header">>, ?DEFAULT_CONTENT_TYPE_HDR)).

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
start_link(_) ->
    _ = ssl:start(),
    gen_server:start_link(?MODULE, [], []).

lookup(Number) when is_binary(Number) ->
    Num = wnm_util:normalize_number(Number),
    lookup(wh_json:set_value(<<"phone_number">>, wh_util:uri_encode(Num), wh_json:new()));
lookup(JObj) ->
    CNAM = case get_cnam(JObj) of
               <<>> -> wh_json:get_value(<<"Caller-ID-Name">>, JObj, <<"UNKNOWN">>);
               Else -> Else
           end,
    Props = [{<<"Caller-ID-Name">>, CNAM}
             ,{[<<"Custom-Channel-Vars">>, <<"Caller-ID-Name">>], CNAM}
            ],
    wh_json:set_values(Props, JObj).

get_cnam(JObj) ->
    Number = wh_json:get_value(<<"Caller-ID-Number">>, JObj, <<"0000000000">>),
    Num = wnm_util:normalize_number(Number),
    case wh_cache:fetch_local(?STEPSWITCH_CACHE, cache_key(Num)) of
        {'ok', CNAM} -> CNAM;
        {'error', 'not_found'} ->
            fetch_cnam(Num, wh_json:set_value(<<"phone_number">>, wh_util:uri_encode(Num), JObj))
    end.

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
    TemplateName = wh_util:to_atom(couch_mgr:get_uuid(), 'true'),
    {'ok', TemplateName}.

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
handle_call({'render', Props, Template}, _, TemplateName) ->
    {'ok', TemplateName} = erlydtl:compile(Template, TemplateName),
    {'ok', Result} = TemplateName:render(Props),
    {'reply', {'ok', Result}, TemplateName};
handle_call(_Request, _From, TemplateName) ->
    {'reply', {'error', 'not_implemented'}, TemplateName}.

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
handle_cast(_Msg, TemplateName) ->
    {'noreply', TemplateName}.

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
handle_info(_Info, TemplateName) ->
    {'noreply', TemplateName}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Allows listener to pass options to handlers
%%
%% @spec handle_event(JObj, State) -> {reply, Options}
%% @end
%%--------------------------------------------------------------------
handle_event(_JObj, _TemplateName) ->
    {'reply', []}.

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
terminate(_Reason, _TemplateName) ->
    lager:debug("stepswitch cnam worker terminating: ~p", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, TemplateName, _Extra) ->
    {'ok', TemplateName}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec json_to_template_props(api_object()) -> 'undefined' | wh_proplist().
json_to_template_props('undefined') -> 'undefined';
json_to_template_props(JObj) ->
    normalize_proplist(wh_json:recursive_to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_proplist(wh_proplist()) -> wh_proplist().
normalize_proplist(Props) -> [normalize_proplist_element(Elem) || Elem <- Props].

normalize_proplist_element({K, V}) when is_list(V) ->
    {normalize_value(K), normalize_proplist(V)};
normalize_proplist_element({K, V}) when is_binary(V) ->
    {normalize_value(K), mochiweb_html:escape(V)};
normalize_proplist_element({K, V}) ->
    {normalize_value(K), V};
normalize_proplist_element(Else) ->
    Else.

normalize_value(Value) -> binary:replace(wh_util:to_lower_binary(Value), <<"-">>, <<"_">>, ['global']).

cache_key(Number) -> {'cnam', Number}.

fetch_cnam(Number, JObj) ->
    CNAM = make_request(Number, JObj),
    CacheProps = [{'expires', whapps_config:get_integer(?CONFIG_CAT, <<"cnam_expires">>, ?DEFAULT_EXPIRES)}],
    wh_cache:store_local(?STEPSWITCH_CACHE, cache_key(Number), CNAM, CacheProps),
    CNAM.

make_request(Number, JObj) ->
    Url = get_http_url(JObj),
    Body = get_http_body(JObj),
    Method = get_http_method(),
    Headers = get_http_headers(),
    HTTPOptions = get_http_options(Url),
    case ibrowse:send_req(Url, Headers, Method, Body, HTTPOptions, 1500) of
        {'ok', Status, _, ResponseBody} when size (ResponseBody) > 18 ->
            lager:debug("cnam lookup for ~p returned ~p: ~p", [Number, Status, ResponseBody]),
            binary:part(ResponseBody, 0, 18);
        {'ok', Status, _, ResponseBody} ->
            lager:debug("cnam lookup for ~p returned ~p: ~p", [Number, Status, ResponseBody]),
            ResponseBody;
        {'error', _R} ->
            lager:debug("cnam lookup for ~p failed: ~p", [Number, _R]),
            <<>>
    end.

-spec get_http_url(wh_json:object()) -> list().
get_http_url(JObj) ->
    Template = whapps_config:get_binary(?CONFIG_CAT, <<"http_url">>, ?DEFAULT_URL),
    case binary:match(Template, <<"opencnam">>) of
        'nomatch' ->
            {'ok', Url} = render(JObj, Template),
            lists:flatten(Url);
        _Else ->
            {'ok', Url} = render(JObj, Template),
            case mochiweb_util:urlsplit(wh_util:to_list(Url)) of
                {_Scheme, _Host, _Path, "", _Segment} ->
                    lists:flatten([Url, "?ref=2600hz&format=pbx"]);
                {Scheme, Host, Path, QS, Segment} ->
                    mochiweb_util:urlunsplit({Scheme, Host, Path
                                              ,[QS, "&ref=2600hz&format=pbx"]
                                              ,Segment
                                             })
            end
    end.

-spec get_http_body(wh_json:object()) -> list().
get_http_body(JObj) ->
    Template = whapps_config:get_binary(?CONFIG_CAT, <<"http_body">>, ?DEFAULT_CONTENT),
    case wh_util:is_empty(Template) of
        'true' -> [];
        'false' ->
            {'ok', Body} = render(JObj, Template),
            lists:flatten(Body)
    end.

get_http_headers() ->
    [{"Accept", ?HTTP_ACCEPT_HEADER}
     ,{"User-Agent", ?HTTP_USER_AGENT}
     ,{"Content-Type", ?HTTP_CONTENT_TYPE}
    ].

get_http_options(Url) ->
    Defaults = [{'response_format', 'binary'}
                ,{'connect_timeout', 500}
                ,{'inactivity_timeout', 1500}
               ],
    Routines = [fun maybe_enable_ssl/2
                ,fun maybe_enable_auth/2
               ],
    lists:foldl(fun(F, P) -> F(Url, P) end, Defaults, Routines).

maybe_enable_ssl(<<"https", _/binary>>, Props) ->
    [{'ssl', [{'verify', 0}]}|Props];
maybe_enable_ssl(_, Props) -> Props.

maybe_enable_auth(_, Props) ->
    Username = whapps_config:get_string(?CONFIG_CAT, <<"http_basic_auth_username">>, <<>>),
    Password = whapps_config:get_string(?CONFIG_CAT, <<"http_basic_auth_password">>, <<>>),
    case wh_util:is_empty(Username) orelse wh_util:is_empty(Password) of
        'true' -> Props;
        'false' -> [{'basic_auth', {Username, Password}}|Props]
    end.

get_http_method() ->
    case whapps_config:get_binary(?CONFIG_CAT, <<"http_method">>, ?DEFAULT_METHOD) of
        <<"post">> -> 'post';
        <<"put">> -> 'put';
        _Else -> 'get'
    end.

-spec render(wh_json:object(), ne_binary()) ->
                    {'ok', iolist()} |
                    {'error', 'timeout'}.
render(JObj, Template) ->
    case catch poolboy:checkout(?STEPSWITCH_CNAM_POOL, 'false', 1000) of
        W when is_pid(W) ->
            Props = json_to_template_props(JObj),
            Reply = gen_server:call(W, {'render', Props, Template}),
            poolboy:checkin(?STEPSWITCH_CNAM_POOL, W),
            Reply;
        _Else -> {'error', 'timeout'}
    end.
