%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(wh_media_file).

-export([get_uri/2]).

-include("whistle_media.hrl").

-spec get_uri(ne_binaries() | ne_binary(), wh_json:object()) ->
                     {'ok', ne_binary()} |
                     {'error', 'not_found'} |
                     {'error', 'no_data'} |
                     {'error', 'no_stream_strategy'}.
get_uri(Media, JObj) when is_binary(Media) ->
    wh_util:put_callid(JObj),
    Paths = [Path
             || Path <- binary:split(Media, <<"/">>, ['global', 'trim']),
                (not wh_util:is_empty(Path))
            ],
    get_uri(Paths, JObj);
get_uri(Paths, JObj) ->
    case find_attachment(Paths) of
        {'error', _}=E -> E;
        {'ok', #media_store_path{}=Store} -> maybe_proxy(JObj, Store)
    end.

-spec maybe_prepare_proxy(ne_binary()) -> 'ok' | 'error'.
maybe_prepare_proxy(URI) ->
    case wh_util:to_binary(props:get_value('path', uri_parser:parse(wh_util:to_list(URI), []), <<>>)) of
        <<"/single/", Rest/binary>> -> prepare_proxy(binary:split(Rest, <<"/">>, ['global', 'trim']));
        <<"/continuous/", Rest/binary>> -> prepare_proxy(binary:split(Rest, <<"/">>, ['global', 'trim']));
        _Else -> 'ok'
    end.

-spec prepare_proxy(ne_binaries()) -> 'ok' | 'error'.
prepare_proxy([Db, Id, _Type, _Rev, Attachment]) ->
    case wh_media_cache_sup:find_file_server(Db, Id, Attachment) =:= {'error', 'no_file_server'} of
        'true' -> start_media_file_cache(Db, Id, Attachment);
        'false' -> lager:debug("existing file server for ~s/~s/~s", [Db, Id, Attachment])
    end.

-spec start_media_file_cache(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
start_media_file_cache(Db, Id, Attachment) ->
    case wh_media_cache_sup:start_file_server(Db, Id, Attachment) of
        {'ok', _FileServer} ->
            lager:debug("file server at ~p for ~s/~s/~s", [_FileServer, Db, Id, Attachment]);
        E ->
            lager:debug("failed to start server for ~s/~s/~s: ~p", [Db, Id, Attachment, E]),
            throw(E)
    end.

-spec maybe_proxy(wh_json:object(), media_store_path()) ->
          {'ok', ne_binary()} |
          {'error', 'no_stream_strategy'}.
maybe_proxy(JObj, #media_store_path{db = Db
                                    ,id = Id
                                    ,type = Type
                                    ,att = Attachment
                                   }=Store) ->
    case kz_datamgr:attachment_url(Db, {Type, Id}, Attachment) of
        {'ok', _}=URI -> URI;
        {'proxy', _} -> proxy_uri(JObj, Store)
    end.

-spec proxy_uri(wh_json:object(), media_store_path()) ->
          {'ok', ne_binary()} |
              {'error', 'no_stream_strategy'}.
proxy_uri(JObj, #media_store_path{db = Db
                                  ,id = Id
                                  ,type = Type
                                  ,rev = Rev
                                  ,att = Attachment
                                 }) ->
    Host = wh_network_utils:get_hostname(),
    Port = whapps_config:get_binary(?CONFIG_CAT, <<"proxy_port">>, 24517),
    StreamType = wh_media_util:convert_stream_type(wh_json:get_value(<<"Stream-Type">>, JObj)),
    Permissions = case StreamType =:= <<"store">> of
                      'true' -> 'proxy_store';
                      'false' -> 'proxy_playback'
                  end,
    URL = <<(wh_media_util:base_url(Host, Port, Permissions))/binary
             ,StreamType/binary
              ,"/", Db/binary
              ,"/", Id/binary
              ,"/", Type/binary
              ,"/", Rev/binary
              ,"/", Attachment/binary
          >>,
    _ = maybe_prepare_proxy(URL),
    {'ok', URL}.



-spec find_attachment(ne_binaries() | ne_binary()) ->
                             {'ok', media_store_path()} |
                             {'error', 'not_found'}.
find_attachment(Media) when is_binary(Media) ->
    Paths = [Path
             || Path <- binary:split(Media, <<"/">>, ['global', 'trim']),
                (not wh_util:is_empty(Path))
            ],
    find_attachment(Paths);
find_attachment([Id]) ->
    find_attachment([?WH_MEDIA_DB, Id]);
find_attachment([Db, Id]) ->
    find_attachment([Db, Id, 'first']);
find_attachment([Db, Id, 'first']) ->
    maybe_find_attachment(Db, Id);
find_attachment([Db, Id, Attachment]) ->
    find_attachment([Db, Id, <<"unknown">>, Attachment]);
find_attachment([Db, Id, Type, Attachment]) ->
    {'ok', Rev} = kz_datamgr:lookup_doc_rev(Db, {Type, Id}),
    find_attachment([Db, Id, Type, Rev, Attachment]);
find_attachment([Db = ?MEDIA_DB, Id, Type, Rev, Attachment]) ->
    {'ok', #media_store_path{db = Db
                            ,id = Id
                            ,type = Type
                            ,rev = Rev
                            ,att = Attachment
                            }
    };
find_attachment([?MATCH_ACCOUNT_RAW(Account), Id, Type, Rev, Attachment]) ->
    AccountDb =  wh_util:format_account_id(Account, 'encoded'),
    {'ok', #media_store_path{db = AccountDb
                            ,id = Id
                            ,type = Type
                            ,rev = Rev
                            ,att = Attachment
                            }
    };
find_attachment([?MATCH_ACCOUNT_UNENCODED(Account), Id, Type, Rev, Attachment]) ->
    AccountDb =  wh_util:format_account_id(Account, 'encoded'),
    {'ok', #media_store_path{db = AccountDb
                            ,id = Id
                            ,type = Type
                            ,rev = Rev
                            ,att = Attachment
                            }
    };
find_attachment([Db, Id, Type, Rev, Attachment]) ->
    {'ok', #media_store_path{db = Db
                            ,id = Id
                            ,type = Type
                            ,rev = Rev
                            ,att = Attachment
                            }
    }.

-spec maybe_find_attachment(ne_binary(), ne_binary()) ->
                                   {'ok', {ne_binary(), ne_binary(), ne_binary()}} |
                                   {'error', 'not_found' | 'no_data'}.
-spec maybe_find_attachment(ne_binary(), ne_binary(), wh_json:object()) ->
                                   {'ok', {ne_binary(), ne_binary(), ne_binary()}} |
                                   {'error', 'not_found' | 'no_data'}.
maybe_find_attachment(?MEDIA_DB = Db, Id) ->
    maybe_find_attachment_in_db(Db, Id);
maybe_find_attachment(Db, Id) ->
    AccountDb = wh_util:format_account_id(Db, 'encoded'),
    maybe_find_attachment_in_db(AccountDb, Id).

-spec maybe_find_attachment_in_db(ne_binary(), ne_binary()) ->
                                         {'ok', {ne_binary(), ne_binary(), ne_binary()}} |
                                         {'error', 'not_found' | 'no_data'}.
maybe_find_attachment_in_db(Db, Id) ->
    case kz_datamgr:open_cache_doc(Db, Id, [{'cache_failures', ['not_found']}]) of
        {'error', _R} ->
            lager:debug("unable to open media doc ~s in ~s: ~p", [Id, Db, _R]),
            {'error', 'not_found'};
        {'ok', JObj} ->
            maybe_find_attachment(Db, Id, JObj)
    end.

maybe_find_attachment(Db, Id, JObj) ->
    lager:debug("trying to find first attachment on doc ~s in db ~s", [Id, Db]),
    case wh_doc:attachment_names(JObj) of
        [] ->
            lager:debug("media doc ~s in ~s has no attachments", [Id, Db]),
            {'error', 'no_data'};
        [AttachmentName | _] ->
            AccountId = wh_util:format_account_id(Db, 'raw'),
            lager:debug("found first attachment ~s on ~s in ~s", [AttachmentName, Id, Db]),
            find_attachment([AccountId, Id, wh_doc:type(JObj), wh_doc:revision(JObj), AttachmentName])
    end.
