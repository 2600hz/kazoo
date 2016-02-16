%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz INC
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

-export([lookup/1
         ,flush/0
        ]).

-include("stepswitch.hrl").

-define(SERVER, ?MODULE).

-define(CONFIG_CAT, <<"stepswitch.cnam">>).

-define(DEFAULT_EXPIRES, 900).
-define(DEFAULT_METHOD, <<"get">>).
-define(DEFAULT_CONTENT, <<>>).
-define(DEFAULT_URL, <<"https://api.opencnam.com/v2/phone/{{phone_number}}">>).
-define(DEFAULT_ACCEPT_HDR, <<"text/pbx,text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8">>).
-define(DEFAULT_USER_AGENT_HDR, <<"Kazoo Stepswitch CNAM">>).
-define(DEFAULT_CONTENT_TYPE_HDR, <<"application/json">>).

-define(HTTP_ACCEPT_HEADER
        ,whapps_config:get_string(?CONFIG_CAT, <<"http_accept_header">>, ?DEFAULT_ACCEPT_HDR)
       ).
-define(HTTP_USER_AGENT
        ,whapps_config:get_string(?CONFIG_CAT, <<"http_user_agent_header">>, ?DEFAULT_USER_AGENT_HDR)
       ).
-define(HTTP_CONTENT_TYPE
        ,whapps_config:get_string(?CONFIG_CAT, <<"http_content_type_header">>, ?DEFAULT_CONTENT_TYPE_HDR)
       ).
-define(HTTP_CONNECT_TIMEOUT_MS
        ,whapps_config:get_integer(?CONFIG_CAT, <<"http_connect_timeout_ms">>, 500)
       ).
-define(DISABLE_NORMALIZE
        ,whapps_config:get_is_true(?CONFIG_CAT, <<"disable_normalize">>, 'false')
       ).

-define(CACHE_KEY(Number), {'cnam', Number}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link(any()) -> startlink_ret().
start_link(_) ->
    _ = ssl:start(),
    gen_server:start_link(?SERVER, [], []).

-spec lookup(wh_json:object() | ne_binary()) -> wh_json:object().
lookup(<<_/binary>> = Number) ->
    Num = case ?DISABLE_NORMALIZE of
              'false' -> wnm_util:normalize_number(Number);
              'true'  -> Number
          end,
    lookup(
      wh_json:from_list(
        [{<<"phone_number">>, wh_util:uri_encode(Num)}
         ,{<<"Caller-ID-Number">>, Num}
        ]
       )
     );
lookup(JObj) ->
    Number = wh_json:get_value(<<"Caller-ID-Number">>, JObj,  wh_util:anonymous_caller_id_number()),
    Num = case ?DISABLE_NORMALIZE of
              'false' -> wnm_util:normalize_number(Number);
              'true'  -> Number
          end,
    case kz_cache:fetch_local(?STEPSWITCH_CACHE, cache_key(Num)) of
        {'ok', CNAM} ->
            update_request(JObj, CNAM, 'true');
        {'error', 'not_found'} ->
            CNAM = fetch_cnam(Num, set_phone_number(Num, JObj)),
            update_request(JObj, CNAM, 'false')
    end.

-spec set_phone_number(ne_binary(), wh_json:object()) -> wh_json:object().
set_phone_number(Num, JObj) ->
    wh_json:set_value(<<"phone_number">>, wh_util:uri_encode(Num), JObj).

-spec update_request(wh_json:object(), api_binary(), boolean()) -> wh_json:object().
update_request(JObj, 'undefined', _) -> JObj;
update_request(JObj, CNAM, FromCache) ->
    Props = [{<<"Caller-ID-Name">>, CNAM}
             ,{[<<"Custom-Channel-Vars">>, <<"Caller-ID-Name">>], CNAM}
             ,{[<<"Custom-Channel-Vars">>, <<"CNAM-From-Cache">>], FromCache}
            ],
    wh_json:set_values(Props, JObj).

-spec flush() -> non_neg_integer().
flush() ->
    kz_cache:filter_erase_local(?STEPSWITCH_CACHE, fun flush_entries/2).

-spec flush_entries(any(), any()) -> boolean().
flush_entries(?CACHE_KEY(_), _) -> 'true';
flush_entries(_, _) -> 'false'.

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
    TemplateName = wh_util:to_atom(kz_datamgr:get_uuid(), 'true'),
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
    {'ok', TemplateName} = erlydtl:compile_template(Template, TemplateName),
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
normalize_proplist(Props) ->
    [normalize_proplist_element(Elem) || Elem <- Props].

-spec normalize_proplist_element({wh_proplist_key(), wh_proplist_value()}) ->
                                        {wh_proplist_key(), wh_proplist_value()}.
normalize_proplist_element({K, V}) when is_list(V) ->
    {normalize_value(K), normalize_proplist(V)};
normalize_proplist_element({K, V}) when is_binary(V) ->
    {normalize_value(K), kz_html:escape(V)};
normalize_proplist_element({K, V}) ->
    {normalize_value(K), V};
normalize_proplist_element(Else) ->
    Else.

-spec normalize_value(binary()) -> binary().
normalize_value(Value) ->
    binary:replace(wh_util:to_lower_binary(Value), <<"-">>, <<"_">>, ['global']).

-spec cache_key(ne_binary()) -> {'cnam', ne_binary()}.
cache_key(Number) -> ?CACHE_KEY(Number).

-spec fetch_cnam(ne_binary(), wh_json:object()) -> api_binary().
fetch_cnam(Number, JObj) ->
    case make_request(Number, JObj) of
        'undefined' -> 'undefined';
        CNAM ->
            CacheProps = [{'expires', whapps_config:get_integer(?CONFIG_CAT, <<"cnam_expires">>, ?DEFAULT_EXPIRES)}],
            kz_cache:store_local(?STEPSWITCH_CACHE, cache_key(Number), CNAM, CacheProps),
            CNAM
    end.

-spec make_request(ne_binary(), wh_json:object()) -> api_binary().
make_request(Number, JObj) ->
    Url = wh_util:to_list(get_http_url(JObj)),
    case kz_http:req(get_http_method()
                     ,Url
                     ,get_http_headers()
                     ,get_http_body(JObj)
                     ,get_http_options(Url)
                    )
    of
        {'ok', 404, _, _} ->
            lager:debug("cnam lookup for ~s returned 404", [Number]),
            'undefined';
        {'ok', Status, _, <<>>} ->
            lager:debug("cnam lookup for ~s returned as ~p and empty body", [Number, Status]),
            'undefined';
        {'ok', Status, _, ResponseBody} when size(ResponseBody) > 18 ->
            lager:debug("cnam lookup for ~s returned ~p: ~s", [Number, Status, ResponseBody]),
            wh_util:truncate_right_binary(ResponseBody, 18);
        {'ok', Status, _, ResponseBody} ->
            lager:debug("cnam lookup for ~s returned ~p: ~s", [Number, Status, ResponseBody]),
            ResponseBody;
        {'error', _R} ->
            lager:debug("cnam lookup for ~s failed: ~p", [Number, _R]),
            'undefined'
    end.

-spec get_http_url(wh_json:object()) -> ne_binary().
get_http_url(JObj) ->
    Template = whapps_config:get_binary(?CONFIG_CAT, <<"http_url">>, ?DEFAULT_URL),
    {'ok', SrcUrl} = render(JObj, Template),
    Url = iolist_to_binary(SrcUrl),

    case binary:match(Template, <<"opencnam">>) of
        'nomatch' -> Url;
        _Else ->
            case kz_http_util:urlsplit(Url) of
                {_Scheme, _Host, _Path, <<>>, _Segment} ->
                    <<Url/binary, "?ref=2600hz&format=pbx">>;
                {Scheme, Host, Path, QS, Segment} ->
                    kz_http_util:urlunsplit({Scheme, Host, Path, <<QS/binary, "&ref=2600hz&format=pbx">>, Segment})
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

-spec get_http_headers() -> wh_proplist().
get_http_headers() ->
    Headers = [{"Accept", ?HTTP_ACCEPT_HEADER}
               ,{"User-Agent", ?HTTP_USER_AGENT}
               ,{"Content-Type", ?HTTP_CONTENT_TYPE}
              ],
    Routines = [
                fun maybe_enable_auth/1
               ],
    lists:foldl(fun(F, P) -> F(P) end, Headers, Routines).

-spec get_http_options(string()) -> wh_proplist().
get_http_options(Url) ->
    Defaults = [{'connect_timeout', ?HTTP_CONNECT_TIMEOUT_MS}
                ,{'timeout', 1500}
               ],
    Routines = [fun maybe_enable_ssl/2
               ],
    lists:foldl(fun(F, P) -> F(Url, P) end, Defaults, Routines).

-spec maybe_enable_ssl(ne_binary(), wh_proplist()) -> wh_proplist().
maybe_enable_ssl(<<"https", _/binary>>, Props) ->
    [{'ssl', [{'verify', 'verify_none'}]}|Props];
maybe_enable_ssl(_, Props) -> Props.

-spec maybe_enable_auth(wh_proplist()) -> wh_proplist().
maybe_enable_auth(Props) ->
    Username = whapps_config:get_string(?CONFIG_CAT, <<"http_basic_auth_username">>, <<>>),
    Password = whapps_config:get_string(?CONFIG_CAT, <<"http_basic_auth_password">>, <<>>),
    case wh_util:is_empty(Username) orelse wh_util:is_empty(Password) of
        'true' -> Props;
        'false' -> [ basic_auth(Username, Password) | Props]
    end.

-spec basic_auth(string(), string()) -> {string(), string()}.
basic_auth(Username, Password) ->
    Encoded = base64:encode_to_string(Username ++ [$: | Password]),
    {"Authorization", lists:flatten(["Basic ", Encoded])}.

-spec get_http_method() -> 'get' | 'put' | 'post'.
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
