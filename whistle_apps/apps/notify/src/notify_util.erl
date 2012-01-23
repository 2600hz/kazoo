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

-include("notify.hrl").

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
    {normalize_value(K), normalize_value(V)};
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

