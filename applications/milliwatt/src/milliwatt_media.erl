%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(milliwatt_media).

-behaviour(gen_server).

-export([start_link/4
         ,stop/1
        ]).

-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("milliwatt.hrl").

-record(state, {username
                ,password
                ,account_name
                ,file_to_upload
                ,auth_token
                ,account_id
                ,media_id
                ,crossbar_url = <<"http://thinky64.2600hz.com:8000">>
               }).

start_link(User, Pass, AccountName, FileToUpload) ->
    gen_server:start_link(?MODULE, [User, Pass, AccountName, FileToUpload], []).

stop(Pid) ->
    gen_server:cast(Pid, 'stop').

init([User, Pass, AccountName, FileToUpload]) ->
    gen_server:cast(self(), 'create_auth_token'),
    {'ok', #state{username=User
                  ,password=Pass
                  ,account_name=AccountName
                  ,file_to_upload=FileToUpload
                 }}.

handle_call(Msg, _From, State) ->
    {'reply', Msg, State}.

handle_cast('create_auth_token', State) ->
    {'ok', AuthToken, AccountId} = create_auth_token(State),
    gen_server:cast(self(), 'create_media_meta'),
    {'noreply', State#state{auth_token=AuthToken
                            ,account_id=AccountId
                           }};
handle_cast('create_media_meta', State) ->
    {'ok', MetaJObj} = create_media_meta(State),
    gen_server:cast(self(), 'upload_media_binary'),
    {'noreply', State#state{media_id=wh_json:get_value(<<"id">>, MetaJObj)}};
handle_cast('upload_media_binary', State) ->
    'ok' = upload_media_binary(State),
    {'stop', 'normal', State};
handle_cast('stop', State) ->
    {'stop', 'normal', State};
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

handle_info(_Msg, State) ->
    lager:debug("unhandled info: ~p", [_Msg]),
    {'noreply', State}.

terminate(_Reason, State) ->
    cleanup_media(State),
    lager:debug("server is terminating: ~p", [_Reason]).

code_change(_OldState, State, _Extra) ->
    {'ok', State}.

create_auth_token(#state{username=User
                         ,password=Pass
                         ,account_name=AccountName
                         ,crossbar_url=Url
                        }) ->
    AuthJObj = wh_json:from_list([{<<"credentials">>, wh_util:binary_md5(<<User/binary, ":", Pass/binary>>)}
                                  ,{<<"account_name">>, AccountName}
                                 ]),
    {200, _RespHeaders, RespBody} =
        do_put(<<Url/binary, "/v1/user_auth">>
               ,[{"Content-Type", "application/json"}]
               ,AuthJObj
              ),
    Envelope = wh_json:decode(RespBody),
    AuthToken = wh_json:get_value(<<"auth_token">>, Envelope),
    AccountId = wh_json:get_value([<<"data">>, <<"account_id">>], Envelope),
    {'ok', AuthToken, AccountId}.

create_media_meta(#state{auth_token=AuthToken
                         ,account_id=AccountId
                         ,crossbar_url=Url
                         ,file_to_upload=UploadPath
                        }) ->
    Name = wh_util:rand_hex_binary(4),
    lager:debug("creating media meta ~s for ~s", [Name, UploadPath]),

    MediaData = wh_json:set_values([{<<"name">>, Name}
                                    ,{<<"description">>, UploadPath}
                                   ], wh_json:new()),

    {200, _RespHeaders, RespBody} =
        do_put(<<Url/binary, "/v1/accounts/", AccountId/binary, "/media">>
                   ,[{"Content-Type", "application/json"}
                     ,{"X-Auth-Token", wh_util:to_list(AuthToken)}
                    ]
               ,MediaData
              ),
    Envelope = wh_json:decode(RespBody),
    {'ok', wh_json:get_value(<<"data">>, Envelope)}.

cleanup_media(#state{auth_token=AuthToken
                     ,account_id=AccountId
                     ,crossbar_url=Url
                     ,media_id=MediaId
                    }) ->
    {200, _RespHeaders, _RespBody} =
        do_delete(<<Url/binary, "/v1/accounts/", AccountId/binary, "/media/", MediaId/binary>>
                  ,[{"X-Auth-Token", wh_util:to_list(AuthToken)}]
                 ).

upload_media_binary(#state{auth_token=AuthToken
                           ,account_id=AccountId
                           ,crossbar_url=Url
                           ,media_id=MediaId
                           ,file_to_upload=UploadPath
                          }) ->
    {'ok', File} = file:read_file(UploadPath),
    <<".", Extension/binary>> = filename:extension(UploadPath),

    {200, _RespHeaders, _RespBody} =
        do_upload(<<Url/binary, "/v1/accounts/", AccountId/binary, "/media/", MediaId/binary, "/raw">>
                  ,[{"X-Auth-Token", wh_util:to_list(AuthToken)}
                    ,{"Content-Type", ext_to_mimetype(Extension)}
                   ]
                  ,File
                 ),
    'ok'.

ext_to_mimetype(<<"mp3">>) ->
    "audio/mpeg";
ext_to_mimetype(<<"wav">>) ->
    "audio/x-wav".

do_upload(Url, ReqHeaders, Binary) ->
    do_req(Url, ReqHeaders, 'post', Binary).

do_delete(Url, ReqHeaders) ->
    do_req(Url, ReqHeaders, 'delete', []).

do_put(Url, ReqHeaders, DataJObj) ->
    Envelope = wh_json:from_list([{<<"data">>, DataJObj}]),
    do_req(Url, ReqHeaders, 'put', wh_json:encode(Envelope)).

do_req(Url, ReqHeaders, Method, ReqBody) ->
    case ibrowse:send_req(wh_util:to_list(Url)
                          ,ReqHeaders
                          ,Method
                          ,ReqBody
                          ,[{'response_format', 'binary'}]
                          )
    of
        {'ok', RespCode, RespHeaders, RespBody} ->
            RequestId = props:get_value("x-request-id", RespHeaders),
            lager:debug("~s: ~s for ~s resp: ~s", [Method, RespCode, RequestId, RespBody]),
            {wh_util:to_integer(RespCode), RespHeaders, RespBody};
        {'error', _E}=Error ->
            lager:debug("~s error for ~s: ~p", [Method, Url, _E]),
            Error
    end.
