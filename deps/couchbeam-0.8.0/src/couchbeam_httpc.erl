%%% -*- erlang -*-
%%%
%%% This file is part of couchbeam released under the MIT license.
%%% See the NOTICE for more information.

-module(couchbeam_httpc).

-include_lib("ibrowse/include/ibrowse.hrl").
-include_lib("whistle/include/wh_types.hrl").

-export([request/4, request/5, request/6,
        request_stream/4, request_stream/5, request_stream/6,
        clean_mailbox_req/1,
        redirect_url/2]).

-define(TIMEOUT, ?MILLISECONDS_IN_MINUTE * 5).

%% @doc send an ibrowse request
-type error_possibilities() ::
        {ok, _, _, _} |
        {url_parsing_failed, invalid_uri} |
        {conn_failed, _} |
        {send_failed, _} |
        retry_later |
        req_timedout |
        sel_conn_closed |
        worker_id_dead |
        unknown_req_id |
        {'EXIT', _} |
        any().

-spec request/6 :: (atom(), string() | binary(), list(), list(), list(), iolist()) ->
                           {ok, _, _, _} |
                           {ibrowse_req_id, _} |
                           {error, error_possibilities()}.
request(Method, Url, Expect, Options) ->
    request(Method, Url, Expect, Options, [], []).
request(Method, Url, Expect, Options, Headers) ->
    request(Method, Url, Expect, Options, Headers, []).
request(Method, Url, Expect, Options, Headers, Body) ->
    Accept = {"Accept", "application/json, */*;q=0.9"},
    {Headers1, Options1} = maybe_oauth_header(Method, Url, Headers, Options),
    case ibrowse:send_req(Url, [Accept|Headers1], Method, Body,
            [{response_format, binary}|Options1], ?TIMEOUT) of
        Resp={ok, Status, _, _} ->
            case lists:member(Status, Expect) of
                true -> Resp;
                false -> {error, Resp}
            end;
        Error -> Error
    end.

-spec request_stream/6 :: ({pid(), 'once'} | pid(), atom(), string() | binary(), list(), list(), iolist()) ->
                                  {ok, {integer(), integer(), integer()}} |
                                  {error, error_possibilities()}.

%% @doc stream an ibrowse request
request_stream(Pid, Method, Url, Options) ->
    request_stream(Pid, Method, Url, Options, []).
request_stream(Pid, Method, Url, Options, Headers) ->
    request_stream(Pid, Method, Url, Options, Headers, []).
request_stream(Pid, Method, Url, Options, Headers, Body) ->
    {Headers1, Options1} = maybe_oauth_header(Method, Url, Headers,
        Options),
    {ok, ReqPid} = ibrowse_http_client:start_link(Url),

    case ibrowse:send_req_direct(ReqPid, Url, Headers1, Method, Body,
                                 [{stream_to, Pid},
                                  {response_format, binary},
                                  {inactivity_timeout, infinity}|Options1],
                                 ?TIMEOUT) of
        {ibrowse_req_id, ReqId} -> {ok, ReqId};
        Error -> Error
    end.

maybe_oauth_header(Method, Url, Headers, Options) ->
    case couchbeam_util:get_value(oauth, Options) of
        undefined ->
            {Headers, Options};
        OauthProps ->
            Hdr = couchbeam_util:oauth_header(Url, Method, OauthProps),
            {[Hdr|Headers], proplists:delete(oauth, Options)}
    end.


clean_mailbox_req(ReqId) ->
    receive
        {ibrowse_async_response, ReqId, _O} ->
            clean_mailbox_req(ReqId);
        {ibrowse_async_response_end, ReqId} ->
            clean_mailbox_req(ReqId)
    after 0 ->
            ok
    end.

redirect_url(RespHeaders, OrigUrl) ->
    MochiHeaders = mochiweb_headers:make(RespHeaders),
    Location = mochiweb_headers:get_value("Location", MochiHeaders),
    #url{
        host = Host,
        host_type = HostType,
        port = Port,
        path = Path,  % includes query string
        protocol = Proto
    } = ibrowse_lib:parse_url(Location),
    #url{
        username = User,
        password = Passwd
    } = ibrowse_lib:parse_url(OrigUrl),
    Creds = case is_list(User) andalso is_list(Passwd) of
    true ->
        User ++ ":" ++ Passwd ++ "@";
    false ->
        []
    end,
    HostPart = case HostType of
    ipv6_address ->
        "[" ++ Host ++ "]";
    _ ->
        Host
    end,
    atom_to_list(Proto) ++ "://" ++ Creds ++ HostPart ++ ":" ++
        integer_to_list(Port) ++ Path.
