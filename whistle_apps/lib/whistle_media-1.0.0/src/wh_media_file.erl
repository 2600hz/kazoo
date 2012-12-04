%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_file).

-export([get_uri/2]).
-export([maybe_prepare_proxy/1]).

-include("whistle_media.hrl").

-spec get_uri/2 :: ([ne_binary(),...] | ne_binary(), wh_json:object()) -> {'error', 'not_found'} | 
                                                                          {'error', 'no_data'} |
                                                                          {'error', 'no_stream_strategy'}|
                                                                          {'ok', ne_binary()}.
get_uri(Media, JObj) when is_binary(Media) ->
    Paths = [Path 
             || Path <- binary:split(Media, <<"/">>, [global, trim])
                    ,(not wh_util:is_empty(Path))
            ],
    get_uri(Paths, JObj);
get_uri(Paths, JObj) ->        
    case find_attachment(Paths) of
        {error, _}=E -> E;
        {Db, Id, Attachment, _} ->
            DbName = wh_util:format_account_id(Db, raw),
            maybe_local_haproxy_uri(JObj, DbName, Id, Attachment)
    end.            
            
maybe_prepare_proxy(URI) ->
    case wh_util:to_binary(props:get_value(path, uri_parser:parse(wh_util:to_list(URI), []), <<>>)) of
        <<"/single/", Rest/binary>> -> prepary_proxy(binary:split(Rest, <<"/">>, [global, trim]));
        <<"/continuous/", Rest/binary>> -> prepary_proxy(binary:split(Rest, <<"/">>, [global, trim]));
        _Else -> ok
    end.    

prepary_proxy([Db, Id, Attachment]=Tokens) ->
    case wh_media_cache_sup:find_file_server(Db, Id, Attachment) =:= {error, no_file_server}
        andalso find_attachment(Tokens)
    of
        {_, _, _, _}=Media ->            
            start_media_file_cache(Media);
        false ->
            lager:debug("existing file server for ~s/~s/~s", [Db, Id, Attachment]);
        _R ->
            lager:debug("unable to prepare file server for ~s/~s/~s: ~p", [Db, Id, Attachment, _R]),
            error
    end.             

start_media_file_cache({Db, Id, Attachment, MetaData}) ->
    DbName = wh_util:format_account_id(Db, raw),
    {ok, _FileServer} = wh_media_cache_sup:find_file_server(DbName, Id, Attachment, MetaData),
    lager:debug("file server at ~p for ~s/~s/~s", [_FileServer, Db, Id, Attachment]).

find_attachment([Id]) ->
    find_attachment([?MEDIA_DB, Id]);
find_attachment([DbName, Id]) ->
    find_attachment([DbName, Id, first]);
find_attachment([DbName, Id, Attachment]) ->
    Db = case DbName =/= ?MEDIA_DB of
             false-> DbName;
             true -> wh_util:format_account_id(DbName, encoded)
         end,
    case couch_mgr:open_doc(Db, Id) of
        {ok, JObj} -> maybe_find_attachment(JObj, Db, Id, Attachment);
        _ -> {error, not_found}
    end;
find_attachment(Id) when not is_list(Id) ->
    find_attachment([Id]).

maybe_find_attachment(JObj, Db, Id, first) ->
    lager:debug("trying to find first attachment on doc ~s in db ~s", [Id, Db]),
    case wh_json:get_value(<<"_attachments">>, JObj, []) of
        [] -> {error, no_data};
        Attachments ->
            {Attachment, MetaData} = hd(wh_json:to_proplist(Attachments)),
            {Db, Id, Attachment, MetaData}
    end;
maybe_find_attachment(JObj, Db, Id, Attachment) ->
    case wh_json:get_value([<<"_attachments">>, Attachment], JObj, false) of
        undefined -> {error, no_data};
        MetaData -> {Db, Id, Attachment, MetaData}
    end.

maybe_local_haproxy_uri(JObj, Db, Id, Attachment) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_bigcouch_direct">>, false) of
        false -> maybe_media_manager_proxy_uri(JObj, Db, Id, Attachment);
        true ->
            DefaultHost = case couch_mgr:get_host() of
                              "localhost" -> wh_network_utils:get_hostname();
                              Else -> Else
                          end,
            Host = whapps_config:get_binary(?CONFIG_CAT, <<"bigcouch_host">>, wh_util:to_binary(DefaultHost)),
            Port = whapps_config:get_binary(?CONFIG_CAT, <<"bigcouch_port">>, couch_mgr:get_port()),
            Permissions = case wh_media_util:convert_stream_type(wh_json:get_value(<<"Stream-Type">>, JObj)) 
                              =:= <<"store">> 
                          of
                              true -> direct_store;
                              false -> direct_playback
                          end,
            {ok, <<(wh_media_util:base_url(Host, Port, Permissions))/binary, Db/binary
                   ,"/", Id/binary, "/", Attachment/binary>>}
    end.

maybe_media_manager_proxy_uri(JObj, Db, Id, Attachment) ->
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_media_proxy">>, true) of
        false -> 
            lager:warning("unable to build URL for media ~s ~s ~s", [Db, Id, Attachment]),
            {error, no_stream_strategy}; 
        true ->
            Host = wh_network_utils:get_hostname(),
            Port = whapps_config:get_binary(?CONFIG_CAT, <<"proxy_port">>, 24517),
            StreamType = wh_media_util:convert_stream_type(wh_json:get_value(<<"Stream-Type">>, JObj)),            
            Permissions = case StreamType =:= <<"store">> of
                              true -> proxy_store;
                              false -> proxy_playback
                          end,
            {ok, <<(wh_media_util:base_url(Host, Port, Permissions))/binary, StreamType/binary, "/"
                   ,Db/binary, "/", Id/binary, "/", Attachment/binary>>}
    end.
