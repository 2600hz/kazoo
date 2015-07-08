%% -------------------------------------------------------------------
%%
%% Copyright (c) 2013 Carlos Gonzalez Florido.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc UAC Request Generation
-module(nksip_call_uac_make).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').

-export([make/4, proxy_make/2, make_cancel/2, make_ack/2, make_ack/1]).
-include("nksip.hrl").
 

%% ===================================================================
%% Private
%% ===================================================================


%% @doc Generates a new request.
-spec make(nksip:app_id(), nksip:method(), nksip:user_uri(), nksip:optslist()) ->    
    {ok, nksip:request(), nksip:optslist()} | {error, term()}.
    
make(AppId, Method, Uri, Opts) ->
    try
        case nksip_parse:uris(Uri) of
            [RUri] -> ok;
            _ -> RUri = throw(invalid_uri)
        end,
        case nksip_parse:uri_method(RUri, Method) of
            {Method1, RUri1} -> ok;
            error -> Method1 = RUri1 = throw(invalid_uri)
        end,
        FromTag = nksip_lib:uid(),
        DefFrom = case AppId:config_from() of
            undefined ->
                #uri{user = <<"user">>, domain = <<"nksip">>, 
                     ext_opts = [{<<"tag">>, FromTag}]};
            #uri{ext_opts=FromOpts}=ConfigFrom ->
                ConfigFrom#uri{
                    ext_opts=nksip_lib:store_value(<<"tag">>, FromTag, FromOpts)}
        end,
        DefTo = RUri1#uri{port=0, opts=[], headers=[], ext_opts=[], ext_headers=[]},
        % We select only first Call-ID
        CallId = case nksip_lib:get_value(call_id, Opts) of
            undefined -> nksip_lib:luid();
            CallId0 -> CallId0
        end,
        Req1 = #sipmsg{
            id = nksip_lib:uid(),
            class = {req, Method1},
            app_id = AppId,
            ruri = RUri1#uri{headers=[], ext_opts=[], ext_headers=[]},
            from = {DefFrom, FromTag},
            to = {DefTo, <<>>},
            call_id = CallId,
            cseq = {nksip_config:cseq(), Method1},
            forwards = 70,
            transport = #transport{},
            start = nksip_lib:l_timestamp()
        },
        Opts1 = case AppId:config_route() of
            [] -> Opts;
            DefRoutes -> [{route, DefRoutes}|Opts]
        end,
        {Req2, Opts2} = parse_plugin_opts(Req1, Opts1),
        Req3 = case RUri of
            #uri{headers=[]} -> Req2;
            #uri{headers=Headers} -> nksip_parse_header:headers(Headers, Req2, post)
        end,
        {Req4, Opts4} = parse_opts(Opts2, Req3, []),
        Opts5 = case 
            (Method=='INVITE' orelse Method=='SUBSCRIBE' orelse
             Method=='NOTIFY' orelse Method=='REFER' orelse Method=='UPDATE')
            andalso Req4#sipmsg.contacts==[]
            andalso not lists:member(contact, Opts4)
        of  
            true -> [contact|Opts4];
            false -> Opts4
        end,
        {ok, Req4, Opts5}
    catch
        throw:Throw -> {error, Throw}
    end.


%% @private 
-spec proxy_make(nksip:request(), nksip:optslist()) ->    
    {ok, nksip:request(), nksip:optslist()} | {error, term()} | 
    {reply, nksip:sipreply()}.
    
proxy_make(#sipmsg{app_id=AppId, ruri=RUri}=Req, Opts) ->
    try
        {Req1, Opts1} = parse_plugin_opts(Req, Opts),
        Req2 = case RUri of
            #uri{headers=[]} -> Req1;
            #uri{headers=Headers} -> nksip_parse_header:headers(Headers, Req1, post)
        end,
        {Req3, Opts3} = parse_opts(Opts1, Req2, []),
        case AppId:nkcb_uac_proxy_opts(Req3, Opts3) of
            {continue, [Req4, Opts4]} ->
                Req5 = remove_local_routes(Req4),
                {ok, Req5, Opts4};
            {reply, Reply} ->
                {reply, Reply}
        end
    catch
        throw:Throw -> {error, Throw}
    end.


%% @private
remove_local_routes(#sipmsg{app_id=AppId, routes=Routes}=Req) ->
    case do_remove_local_routes(AppId, Routes) of
        Routes -> Req;
        Routes1 -> Req#sipmsg{routes=Routes1}
    end.


%% @private
do_remove_local_routes(_AppId, []) ->
    [];

do_remove_local_routes(AppId, [Route|RestRoutes]) ->
    case nksip_transport:is_local(AppId, Route) of
        true -> do_remove_local_routes(AppId, RestRoutes);
        false -> [Route|RestRoutes]
    end.




%% @doc Generates a <i>CANCEL</i> request from an <i>INVITE</i> request.
-spec make_cancel(nksip:request(), nksip:optslist()) ->
    nksip:request().

make_cancel(Req, Opts) ->
    #sipmsg{
        class = {req, _}, 
        cseq = {CSeq, _}, 
        vias = [Via|_], 
        headers = Hds
    } = Req,
    Headers1 = nksip_lib:extract(Hds, <<"route">>),
    Headers2 = case nksip_lib:get_value(reason, Opts) of
        undefined ->
            Headers1;
        Reason ->
            case nksip_unparse:error_reason(Reason) of
                error -> Headers1;
                BinReason -> [{<<"reason">>, BinReason}|Headers1]
            end
    end,
    Req#sipmsg{
        class = {req, 'CANCEL'},
        id = nksip_lib:uid(),
        cseq = {CSeq, 'CANCEL'},
        forwards = 70,
        vias = [Via],
        headers = Headers2,
        contacts = [],
        content_type = undefined,
        require = [],
        supported = [],
        expires = undefined,
        event = undefined,
        body = <<>>
    }.


%% @doc Generates an <i>ACK</i> request from an <i>INVITE</i> request and a response
-spec make_ack(nksip:request(), nksip:response()) ->
    nksip:request().

make_ack(Req, #sipmsg{to=To}) ->
    make_ack(Req#sipmsg{to=To}).


%% @private
-spec make_ack(nksip:request()) ->
    nksip:request().

make_ack(#sipmsg{vias=[Via|_], cseq={CSeq, _}}=Req) ->
    Req#sipmsg{
        class = {req, 'ACK'},
        id = nksip_lib:uid(),
        vias = [Via],
        cseq = {CSeq, 'ACK'},
        forwards = 70,
        routes = [],
        contacts = [],
        headers = [],
        content_type = undefined,
        require = [],
        supported = [],
        expires = undefined,
        event = undefined,
        body = <<>>
    }.


%% @private
-spec parse_opts(nksip:optslist(), nksip:request(), nksip:optslist()) ->
    {nksip:request(), nksip:optslist()}.


parse_opts([], Req, Opts) ->
    {Req, Opts};

parse_opts([Term|Rest], Req, Opts) ->
    #sipmsg{app_id=AppId, class={req, Method}} = Req,
    Op = case Term of
        
        ignore -> ignore;

        % Header manipulation
        {add, Name, Value} -> {add, Name, Value};
        {add, {Name, Value}} -> {add, Name, Value};
        {replace, Name, Value} -> {replace, Name, Value};
        {replace, {Name, Value}} -> {replace, Name, Value};
        {insert, Name, Value} -> {insert, Name, Value};
        {insert, {Name, Value}} -> {insert, Name, Value};

        {call_id, CallId} when is_binary(CallId) -> ignore;
        {call_id, _} -> error;

        {Name, Value} when Name==from; Name==to; 
                           Name==content_type; Name==require; Name==supported; 
                           Name==expires; Name==contact; Name==route; 
                           Name==reason; Name==event ->
            {replace, Name, Value};

        % Special parameters
        to_as_from ->
            case lists:keymember(from, 1, Rest) of
                false ->
                    #sipmsg{from={From, _}} = Req,
                    {replace, <<"To">>, From#uri{ext_opts=[]}};
                _ ->
                    move_to_last
            end;
        {body, Body} ->
            case lists:keymember(content_type, 1, Rest) of
                false ->
                    ContentType = case Req#sipmsg.content_type of
                        undefined when is_binary(Body) -> undefined;
                        undefined when is_list(Body), is_integer(hd(Body)) -> undefined;
                        undefined when is_record(Body, sdp) -> <<"application/sdp">>;
                        undefined -> <<"application/nksip.ebf.base64">>;
                        CT0 -> CT0
                    end,
                    {update, Req#sipmsg{body=Body, content_type=ContentType}, Opts};
                true ->
                    move_to_last
            end;
        {cseq_num, CSeq} when is_integer(CSeq), CSeq>0, CSeq<4294967296 ->
            #sipmsg{cseq={_, CSeqMethod}} = Req,
            {update, Req#sipmsg{cseq={CSeq, CSeqMethod}}, Opts};
        {cseq_num, _} ->
            error;
        {min_cseq, MinCSeq} ->
            case lists:keymember(cseq_num, 1, Rest) of
                false -> 
                    #sipmsg{cseq={OldCSeq, CSeqMethod}} =Req,
                    case is_integer(MinCSeq) of
                        true when MinCSeq > OldCSeq -> 
                            {update, Req#sipmsg{cseq={MinCSeq, CSeqMethod}}, Opts};
                        true -> 
                            ignore;
                        false -> 
                            error
                    end;
                true ->
                    move_to_last
            end;

        %% Pass-through options
        {no_100, true} ->
            {update, Req, [no_100|Opts]};
        _ when Term==contact; Term==record_route; Term==path; Term==get_request;
               Term==auto_2xx_ack; Term==async; Term==no_100;
               Term==stateless; Term==no_dialog; Term==no_auto_expire;
               Term==follow_redirects ->
            {update, Req, [Term|Opts]};

        {Name, Value} when Name==record_flow; Name==route_flow ->
            {update, Req, [{Name, Value}|Opts]};
        {meta, List} when is_list(List) ->
            case lists:keyfind(meta, 1, Opts) of
                false -> 
                    {update, Req, [{meta, List}|Opts]};
                {meta, List0} ->
                    {update, Req, nksip_lib:store_value(meta, List0++List, Opts)}
            end;
        {meta, _} ->
            error;
        {user, List} when is_list(List) ->
            case lists:keyfind(user, 1, Opts) of
                false -> 
                    {update, Req, [{user, List}|Opts]};
                {user, List0} ->
                    {update, Req, nksip_lib:store_value(user, List0++List, Opts)}
            end;
        {user, _} ->
            error;
        {local_host, auto} ->
            {update, Req, [{local_host, auto}|Opts]};
        {local_host, Host} ->
            {update, Req, [{local_host, nksip_lib:to_host(Host)}|Opts]};
        {local_host6, auto} ->
            {update, Req, [{local_host6, auto}|Opts]};
        {local_host6, Host} ->
            case nksip_lib:to_ip(Host) of
                {ok, HostIp6} -> 
                    % Ensure it is enclosed in `[]'
                    {update, Req, [{local_host6, nksip_lib:to_host(HostIp6, true)}|Opts]};
                error -> 
                    {update, Req, [{local_host6, nksip_lib:to_binary(Host)}|Opts]}
            end;
        {callback, Fun} when is_function(Fun, 1) ->
            {update, Req, [{callback, Fun}|Opts]};
        {callback, _} ->
            error;
        {reg_id, RegId} when is_integer(RegId), RegId>0 ->
            {update, Req, [{reg_id, RegId}|Opts]};
        {reg_id, _} ->
            error;

        %% Automatic header generation (replace existing headers)
        user_agent ->
            {replace, <<"user-agent">>, <<"NkSIP ", ?VERSION>>};
        supported ->
            {replace, <<"supported">>, AppId:config_supported()};
        allow ->        
            {replace, <<"allow">>,  AppId:config_allow()};
        accept ->
            Accept = case AppId:config_accept() of
                undefined when Method=='INVITE'; Method=='UPDATE'; Method=='PRACK' ->
                    <<"application/sdp">>;
                undefined ->
                    <<"*/*">>;
                Accept0 ->
                    Accept0
            end, 
            {replace, <<"accept">>, Accept};
        date ->
            Date = nksip_lib:to_binary(httpd_util:rfc1123_date()),
            {replace, <<"date">>, Date};
        allow_event ->
            case AppId:config_events() of
                [] -> ignore;
                Events -> {replace, <<"allow-event">>, Events}
            end;

        % Register options
        unregister_all ->
            {retry, [{contact, <<"*">>}, {expires, 0}|Rest]};
        unregister ->
            {retry, [contact, {expires, 0}|Rest]};

        % Event options
        {subscription_state, ST} when Method=='NOTIFY'->
            Value = case ST of
                active ->
                    <<"active">>;
                {active, Expires} when is_integer(Expires), Expires>0 ->
                    {<<"active">>, [{<<"expires">>, nksip_lib:to_binary(Expires)}]};
                pending ->
                    <<"pending">>;
                {pending, Expires} when is_integer(Expires), Expires>0 ->
                    {<<"pending">>, [{<<"expires">>, nksip_lib:to_binary(Expires)}]};
                {terminated, Reason} when 
                        Reason==deactivated; Reason==probation; Reason==rejected; 
                        Reason==timeout; Reason==giveup; Reason==noresource; 
                        Reason==invariant ->
                    {<<"terminated">>, [{<<"reason">>, nksip_lib:to_binary(Reason)}]};
                {terminated, Reason, undefined} when 
                        Reason==deactivated; Reason==probation; Reason==rejected; 
                        Reason==timeout; Reason==giveup; Reason==noresource; 
                        Reason==invariant ->
                    {<<"terminated">>, [{<<"reason">>, nksip_lib:to_binary(Reason)}]};
                {terminated, Reason, Retry} when 
                        (Reason==probation orelse Reason==giveup) andalso
                        is_integer(Retry) andalso Retry>0 ->
                    {<<"terminated">>, [
                        {<<"reason">>, nksip_lib:to_binary(Reason)},
                        {<<"retry-after">>, Retry}]};                
                _ ->
                    throw({invalid_config, session_state})
            end,
            {replace, <<"subscription-state">>, Value};
        {subscription_state, _} ->
            error;
        {refer_to, Url} ->
            {replace, "refer-to", Url};

        % Publish options
        {sip_if_match, ETag} ->
            {replace, <<"sip-if-match">>, nksip_lib:to_binary(ETag)};

        _ ->
            {update, Req, [Term|Opts]}
    end,
    case Op of
        {add, AddName, AddValue} ->
            PName = nksip_parse_header:name(AddName), 
            PReq = nksip_parse_header:parse(PName, AddValue, Req, post),
            parse_opts(Rest, PReq, Opts);
        {replace, RepName, RepValue} ->
            PName = nksip_parse_header:name(RepName), 
            PReq = nksip_parse_header:parse(PName, RepValue, Req, replace),
            parse_opts(Rest, PReq, Opts);
        {insert, InsName, InsValue} ->
            PName = nksip_parse_header:name(InsName), 
            PReq = nksip_parse_header:parse(PName, InsValue, Req, pre),
            parse_opts(Rest, PReq, Opts);
        {update, UpdReq, UpdOpts} -> 
            parse_opts(Rest, UpdReq, UpdOpts);
        {retry, Terms} ->
            parse_opts(Terms, Req, Opts);
        move_to_last ->
            parse_opts(Rest++[Term], Req, Opts);
        ignore ->
            parse_opts(Rest, Req, Opts);
        error when is_tuple(Term) ->
            throw({invalid_config, element(1, Term)});
        error ->
            throw({invalid_config, Term})
    end.



%% @private
-spec parse_plugin_opts(nksip:request(), nksip:optslist()) ->
    {nksip:request(), nksip:optslist()}.

parse_plugin_opts(#sipmsg{app_id=AppId}=Req, Opts) ->
    case AppId:nkcb_parse_uac_opts(Req, Opts) of
        {continue, [Req1, Opts1]} ->
            {Req1, Opts1};
        {error, Error} ->
            throw(Error)
    end.


%% ===================================================================
%% EUnit tests
%% ===================================================================

