%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% @end
%%%
%%% @contributors
%%% Karl Anderson <karl@2600hz.org>
%%%
%%% Created : 23 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(notify_util).

-export([send_email/3]).
-export([render_template/3]).
-export([normalize_proplist/1]).
-export([json_to_template_props/1]).
-export([get_service_props/3]).

-include("notify.hrl").
-include_lib("whistle/include/wh_databases.hrl").

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec send_email/3 :: (ne_binary(), ne_binary() | [ne_binary(),...], term()) -> ok.
send_email(From, To, Email) ->
    Encoded = mimemail:encode(Email),
    Relay = wh_util:to_list(whapps_config:get(<<"smtp_client">>, <<"relay">>, <<"localhost">>)),
    ?LOG("sending email to ~s from ~s via ~s", [To, From, Relay]),
    ReqId = get(callid),
    gen_smtp_client:send({From, [To], Encoded}, [{relay, Relay}]
                         ,fun(X) -> ?LOG(ReqId, "email relay responded: ~p", [X]) end).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec json_to_template_props/1 :: (wh_json:json_object()) -> proplist().
json_to_template_props(JObj) ->    
    normalize_proplist(wh_json:recursive_to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec normalize_proplist/1 :: (proplist()) -> proplist().
normalize_proplist(Props) ->
    [normalize_proplist_element(Elem) || Elem <- Props].

normalize_proplist_element({K, V}) when is_list(V) -> 
    {normalize_value(K), normalize_proplist(V)};
normalize_proplist_element({K, V}) -> 
    {normalize_value(K), V};
normalize_proplist_element(Else) ->
    Else.

normalize_value(Value) ->
    binary:replace(wh_util:to_lower_binary(Value), <<"-">>, <<"_">>, [global]).
 
%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec render_template/3 :: (ne_binary() | 'undefined', atom(), proplist()) -> {'ok', iolist()} | {'error', term()}.
render_template(undefined, DefaultTemplate, Props) ->
    ?LOG("rendering default ~s template", [DefaultTemplate]),
    DefaultTemplate:render(Props);
render_template(Template, DefaultTemplate, Props) ->
    try       
        CustomTemplate = wh_util:to_atom(list_to_binary([couch_mgr:get_uuid(), "_"
                                                        ,wh_json:to_binary(DefaultTemplate)
                                                        ])
                                         ,true),
        ?LOG("compiling custom ~s template", [DefaultTemplate]),
        {ok, CustomTemplate} = erlydtl:compile(Template, CustomTemplate),
        ?LOG("rendering custom template ~s", [CustomTemplate]),
        Result = CustomTemplate:render(Props),
        code:purge(CustomTemplate),
        code:delete(CustomTemplate),
        Result
    catch
        _:_E ->
            ?LOG("error compiling custom ~s template: ~p", [DefaultTemplate, _E]),
            render_template(undefined, DefaultTemplate, Props)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% determine the service name, provider, and url. Hunts (in order) 
%% in the event, parent account notification object, and then default.
%% @end
%%--------------------------------------------------------------------
-spec get_service_props/3 :: (wh_json:json_object(), wh_json:json_object(), ne_binary()) -> proplist().
get_service_props(Request, Account, ConfigCat) ->
    DefaultUrl = wh_json:get_ne_value(<<"service_url">>, Request
                                      ,whapps_config:get(ConfigCat, <<"default_service_url">>, <<"http://apps.2600hz.com">>)),
    DefaultName = wh_json:get_ne_value(<<"service_name">>, Request
                                       ,whapps_config:get(ConfigCat, <<"default_service_name">>, <<"VOIP Services">>)),
    DefaultProvider = wh_json:get_ne_value(<<"service_provider">>, Request
                                           ,whapps_config:get(ConfigCat, <<"default_service_provider">>, <<"2600hz">>)),
    DefaultNumber = wh_json:get_ne_value(<<"support_number">>, Request
                                           ,whapps_config:get(ConfigCat, <<"default_support_number">>, <<"(415) 886-7900">>)),
    DefaultEmail = wh_json:get_ne_value(<<"support_email">>, Request
                                        ,whapps_config:get(ConfigCat, <<"default_support_email">>, <<"support@2600hz.com">>)),
    Tree = wh_json:get_value(<<"pvt_tree">>, Account, []),
    [_, Module] = binary:split(ConfigCat, <<".">>),
    case Tree =/= [] andalso couch_mgr:open_doc(?WH_ACCOUNTS_DB, lists:last(Tree)) of
        {ok, JObj} ->
            ?LOG("looking for notifications '~s' service info in: ~s", [Module, wh_json:get_value(<<"_id">>, JObj)]),
            [{<<"url">>, wh_json:get_value([<<"notifications">>, Module, <<"service_url">>], JObj, DefaultUrl)}
             ,{<<"name">>, wh_json:get_value([<<"notifications">>, Module, <<"service_name">>], JObj, DefaultName)}
             ,{<<"provider">>, wh_json:get_value([<<"notifications">>, Module, <<"service_provider">>], JObj, DefaultProvider)}
             ,{<<"support_number">>, wh_json:get_value([<<"notifications">>, Module, <<"support_number">>], JObj, DefaultNumber)}
             ,{<<"support_email">>, wh_json:get_value([<<"notifications">>, Module, <<"support_email">>], JObj, DefaultEmail)}
            ];
        _E ->
            ?LOG("failed to find parent for notifications '~s' service info: ~p", [Module, _E]),
            [{<<"url">>, DefaultUrl}
             ,{<<"name">>, DefaultName}
             ,{<<"provider">>, DefaultProvider}
             ,{<<"support_number">>, DefaultNumber}
             ,{<<"support_email">>, DefaultEmail}
            ]
    end.         
