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

%% @doc UAS Response Generation
-module(nksip_call_uas_make).
-author('Carlos Gonzalez <carlosj.gf@gmail.com>').
-export([make/3]).
-include("nksip.hrl").
-include("nksip_call.hrl").


%% ===================================================================
%% Private
%% ===================================================================



%% @doc Response Generation
-spec make(nksip:request(), nksip:sip_code(), nksip:optslist()) -> 
    {ok, nksip:response(), nksip:optslist()} | {error, term()}.

make(Req, Code, Opts) ->
  #sipmsg{
        class = {req, Method},
        ruri = RUri,
        vias = [LastVia|_] = Vias,
        cseq = {CSeqNum, _},
        to = {#uri{ext_opts=ExtOpts}=To, ToTag},
        routes = Routes,
        contacts = Contacts,
        % require = Require,
        to_tag_candidate = NewToTag
    } = Req, 
    NewToTag1 = case NewToTag of
        <<>> -> nksip_lib:hash(make_ref());
        _ -> NewToTag
    end,
    case Code of
        100 -> 
            Vias1 = [LastVia],
            ExtOpts1 = lists:keydelete(<<"tag">>, 1, ExtOpts),
            {To1, ToTag1} = {To#uri{ext_opts=ExtOpts1}, <<>>};
        _ -> 
            Vias1 = Vias,
            {To1, ToTag1} = case ToTag of
                <<>> -> 
                    ExtOpts1 = [{<<"tag">>, NewToTag1}|ExtOpts],
                    {To#uri{ext_opts=ExtOpts1}, NewToTag1};
                _ -> 
                    {To, ToTag}
            end
    end,
    ReasonPhrase = nksip_lib:get_binary(reason_phrase, Opts),
    Resp1 = Req#sipmsg{
        id = nksip_lib:uid(),
        class = {resp, Code, ReasonPhrase},
        vias = Vias1,
        to = {To1, ToTag1},
        forwards = 70,
        cseq = {CSeqNum, Method},
        routes = [],
        contacts = [],
        headers = [],
        content_type = undefined,
        supported = [],
        require = [],
        expires = undefined,
        event = undefined,
        body = <<>>
    },
    try
        Opts1 = case RUri#uri.scheme of
            sips ->
                [secure|Opts];
            _ ->
                case Routes of
                    [#uri{scheme=sips}|_] -> 
                        [secure|Opts];
                    [] ->
                        case Contacts of
                            [#uri{scheme=sips}|_] -> [secure|Opts];
                            _ -> Opts
                        end;
                    _ ->
                        Opts
                end
        end,
        {Resp2, Opts2} = parse_plugin_opts(Req, Resp1, Opts1),
        {Resp3, Opts3} = parse_opts(Opts2, Req, Resp2, Code, []),
        {ok, Resp3, Opts3}
    catch
        throw:Throw -> {error, Throw}
    end.




%% ===================================================================
%% Internal
%% ===================================================================


%% @private
-spec parse_opts(nksip:optslist(), nksip:request(), nksip:response(), 
                 nksip:sip_code(), nksip:optslist()) -> 
    {nksip:response(), nksip:optslist()}.

parse_opts([], _Req, Resp, _Code, Opts) ->
    {Resp, Opts};


parse_opts([Term|Rest], Req, Resp, Code, Opts) ->
    #sipmsg{app_id=AppId} = Req,
    Op = case Term of
    
        ignore ->
            ignore;
        {pass_through, Pass} ->
            {update, Req, [Pass|Opts]};

         % Header manipulation
        {add, Name, Value} -> 
            {add, Name, Value};
        {add, {Name, Value}} -> 
            {add, Name, Value};
        {replace, Name, Value} -> 
            {replace, Name, Value};
        {replace, {Name, Value}} -> 
            {replace, Name, Value};
        {insert, Name, Value} -> 
            {insert, Name, Value};
        {insert, {Name, Value}} -> 
            {insert, Name, Value};

        {Name, Value} when Name==from; Name==to; Name==content_type; 
                           Name==require; Name==supported; 
                           Name==expires; Name==contact; Name==route; 
                           Name==reason; Name==event ->
            {replace, Name, Value};

        % Already processed
        {Name, _} when Name==reason_phrase ->
            ignore;

        % Special parameters
        timestamp ->
            case nksip_sipmsg:header(<<"timestamp">>, Req, integers) of
                [Time] -> {replace, <<"timestamp">>, Time};
                _ -> ignore
            end;
        {body, Body} ->
            case lists:keymember(content_type, 1, Rest) of
                false ->
                    ContentType = case Resp#sipmsg.content_type of
                        undefined when is_binary(Body) -> undefined;
                        undefined when is_list(Body), is_integer(hd(Body)) -> undefined;
                        undefined when is_record(Body, sdp) -> <<"application/sdp">>;
                        undefined -> <<"application/nksip.ebf.base64">>;
                        CT0 -> CT0
                    end,
                    {update, Resp#sipmsg{body=Body, content_type=ContentType}, Opts};
                true ->
                    move_to_last
            end;
        {to_tag, _} when Code==100 ->
            ignore;
        {to_tag, ToTag} ->
            case is_binary(ToTag) of
                true ->
                    #sipmsg{to={#uri{ext_opts=ExtOpts}=To, _}} = Resp,
                    ExtOpts1 = nksip_lib:store_value(<<"tag">>, ToTag, ExtOpts),
                    {update, Resp#sipmsg{to={To#uri{ext_opts=ExtOpts1}, ToTag}}, Opts};
                false ->
                    throw({invalid_config, to_tag})
            end;

        %% Pass-through options
        % _ when Term==contact; Term==no_dialog; Term==secure; Term==rseq ->
        _ when Term==contact; Term==no_dialog; Term==secure ->
            {update, Resp, [Term|Opts]};
        {local_host, auto} ->
            {update, Resp, [{local_host, auto}|Opts]};
        {local_host, Host} ->
            {update, Resp, [{local_host, nksip_lib:to_host(Host)}|Opts]};
        {local_host6, auto} ->
            {update, Resp, [{local_host6, auto}|Opts]};
        {local_host6, Host} ->
            case nksip_lib:to_ip(Host) of
                {ok, HostIp6} -> 
                    % Ensure it is enclosed in `[]'
                    {update, Resp, [{local_host6, nksip_lib:to_host(HostIp6, true)}|Opts]};
                error -> 
                    {update, Resp, [{local_host6, nksip_lib:to_binary(Host)}|Opts]}
            end;

        %% Automatic header generation (replace existing headers)
        user_agent ->
            {replace, <<"user-agent">>, <<"NkSIP ", ?VERSION>>};
        supported ->
            Supported = AppId:config_supported(),
            {replace, <<"supported">>, Supported};
        allow ->        
            {replace, <<"allow">>, AppId:config_allow()};
        accept ->
            #sipmsg{class={req, Method}} = Req,
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

        % Authentication generation
        www_authenticate ->
            #sipmsg{from={#uri{domain=FromDomain}, _}} = Req,
            {add, <<"www-authenticate">>, nksip_auth:make_response(FromDomain, Req)};
        {www_authenticate, Realm} ->
            case is_binary(Realm) of
                true ->
                    {add, <<"www-authenticate">>, nksip_auth:make_response(Realm, Req)};
                false ->
                    throw({invalid_config, www_authenticate})
            end;
        proxy_authenticate ->
            #sipmsg{from={#uri{domain=FromDomain}, _}} = Req,
            {add, <<"proxy-authenticate">>, nksip_auth:make_response(FromDomain, Req)};
        {proxy_authenticate, Realm} ->
            case is_binary(Realm) of
                true ->
                    {add, <<"proxy-authenticate">>, nksip_auth:make_response(Realm, Req)};
                false ->
                    throw({invalid_config, proxy_authenticate})
            end;
        {service_route, Routes} when Code>=200, Code<300, 
                                     element(2, Req#sipmsg.class)=='REGISTER' ->
            case nksip_parse:uris(Routes) of
                error -> throw({invalid_config, service_route});
                Uris -> {replace, <<"service-route">>, Uris}
            end;
        {service_route, _} ->
            ignore;

        % Publish options
        {sip_etag, ETag} ->
            {replace, <<"sip-etag">>, nksip_lib:to_binary(ETag)};

        _ when is_tuple(Term) ->
            throw({invalid_config, element(1, Term)});
        _ ->
            throw({invalid_config, Term})
    end,
    case Op of
        {add, AddName, AddValue} ->
            PName = nksip_parse_header:name(AddName), 
            PResp = nksip_parse_header:parse(PName, AddValue, Resp, post),
            parse_opts(Rest, Req, PResp, Code, Opts);
        {replace, RepName, RepValue} ->
            PName = nksip_parse_header:name(RepName), 
            PResp = nksip_parse_header:parse(PName, RepValue, Resp, replace),
            parse_opts(Rest, Req, PResp, Code, Opts);
        {insert, InsName, InsValue} ->
            PName = nksip_parse_header:name(InsName), 
            PResp = nksip_parse_header:parse(PName, InsValue, Resp, pre),
            parse_opts(Rest, Req, PResp, Code, Opts);
        {update, UpdResp, UpdOpts} -> 
            parse_opts(Rest, Req, UpdResp, Code, UpdOpts);
        % {retry, RetTerm} ->
        %     parse_opts([RetTerm|Rest], Req, Resp, Code, Opts);
        move_to_last ->
            parse_opts(Rest++[Term], Req, Resp, Code, Opts);
        ignore ->
            parse_opts(Rest, Req, Resp, Code, Opts)
    end.

%% @private
-spec parse_plugin_opts(nksip:request(), nksip:response(), nksip:optslist()) ->
    {nksip:request(), nksip:optslist()}.

parse_plugin_opts(#sipmsg{app_id=AppId}=Req, Resp, Opts) ->
    case AppId:nkcb_parse_uas_opt(Req, Resp, Opts) of
        {continue, [_, Resp1, Opts1]} ->
            {Resp1, Opts1};
        {error, Error} ->
            throw(Error)
    end.

