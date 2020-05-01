%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_media_file).

-export([get_uri/2]).

-include("kazoo_media.hrl").

-type build_uri_args() :: [kz_term:ne_binary() | kz_term:proplist() | atom()].
-type build_uri() :: build_uri_args() | kz_term:ne_binary() | media_store_path().

-spec get_uri(build_uri(), kz_json:object()) ->
          kz_term:ne_binary() |
          {'error', 'not_found'} |
          {'error', 'no_data'} |
          {'error', 'no_stream_strategy'}.
get_uri(#media_store_path{}=Store, JObj) ->
    maybe_proxy(JObj, Store);
get_uri(Media, JObj) when is_binary(Media) ->
    kz_log:put_callid(JObj),
    Paths = [kz_http_util:urldecode(Path)
             || Path <- binary:split(Media, <<"/">>, ['global', 'trim']),
                not kz_term:is_empty(Path)
            ],
    get_uri(Paths, JObj);
get_uri(Paths, JObj) ->
    case find_attachment(Paths) of
        {'error', _}=E -> E;
        {'ok', #media_store_path{}=Store} -> maybe_proxy(JObj, Store)
    end.

-spec maybe_prepare_proxy(kz_term:ne_binary(), media_store_path()) -> 'ok' | 'error'.
maybe_prepare_proxy(<<"single">>, Store) -> prepare_proxy(Store);
maybe_prepare_proxy(<<"continuous">>, Store) -> prepare_proxy(Store);
maybe_prepare_proxy(_, _ ) -> 'ok'.

-spec prepare_proxy(media_store_path()) -> 'ok' | 'error'.
prepare_proxy(#media_store_path{db = Db
                               ,id = Id
                               ,att = Attachment
                               }) ->
    case kz_media_cache_sup:find_file_server(Db, Id, Attachment) =:= {'error', 'no_file_server'} of
        'true' -> start_media_file_cache(Db, Id, Attachment);
        'false' -> lager:debug("existing file server for ~s/~s/~s", [Db, Id, Attachment])
    end.

-spec start_media_file_cache(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
start_media_file_cache(Db, Id, Attachment) ->
    case kz_media_cache_sup:start_file_server(Db, Id, Attachment) of
        {'ok', _FileServer} ->
            lager:debug("file server at ~p for ~s/~s/~s", [_FileServer, Db, Id, Attachment]);
        E ->
            lager:debug("failed to start server for ~s/~s/~s: ~p", [Db, Id, Attachment, E]),
            throw(E)
    end.

-spec maybe_proxy(kz_json:object(), media_store_path()) ->
          kz_term:ne_binary() | {'error', 'no_stream_strategy'}.
maybe_proxy(JObj, #media_store_path{db = Db
                                   ,id = Id
                                   ,att = Attachment
                                   ,opt = Options
                                   }=Store) ->
    lager:debug("fetching attachment url for '~p' , '~p', '~p'", [Db, Id, Attachment]),
    StreamType = kz_media_util:convert_stream_type(kz_json:get_value(<<"Stream-Type">>, JObj)),
    case kz_datamgr:attachment_url(Db, Id, Attachment, [{'stream_type',StreamType} | Options]) of
        {'error', 'not_found'} ->
            lager:debug("no attachment URL found"),
            <<>>;
        {'proxy', _Proxy} ->
            lager:debug("building proxy URI: ~p", [_Proxy]),
            proxy_uri(Store, StreamType);
        <<_/binary>> = URI ->
            lager:debug("using URI ~s", [URI]),
            URI
    end.

-spec proxy_uri(media_store_path(), kz_term:ne_binary()) ->
          kz_term:ne_binary() | {'error', 'no_stream_strategy'}.
proxy_uri(#media_store_path{db = Db
                           ,id = Id
                           ,att = Attachment
                           ,opt=Options
                           }=Store
         ,StreamType
         ) ->
    _ = maybe_prepare_proxy(StreamType, Store),
    Path = kz_http_util:urlencode(base64:encode(term_to_binary({Db, Id, Attachment, Options}))),
    File = kz_http_util:urlencode(Attachment),
    UrlParts = [kz_media_util:proxy_base_url(StreamType)
               ,StreamType
               ,Path
               ,File
               ],
    kz_binary:join(UrlParts, <<"/">>).

-spec find_attachment(build_uri_args() | kz_term:ne_binary()) ->
          {'ok', media_store_path()} |
          {'error', 'not_found'}.
find_attachment(Media) when is_binary(Media) ->
    Paths = [Path
             || Path <- binary:split(Media, <<"/">>, ['global', 'trim']),
                (not kz_term:is_empty(Path))
            ],
    find_attachment(Paths);
find_attachment([Id]) ->
    find_attachment([?KZ_MEDIA_DB, Id]);
find_attachment([Db, Id]) ->
    find_attachment([Db, Id, 'first']);
find_attachment([Db, Id, 'first']) ->
    maybe_find_attachment(Db, Id);
find_attachment([?KZ_MEDIA_DB=Db, Lang, Id]) ->
    find_attachment([Db, <<Lang/binary, "/", Id/binary>>, 'first']);
find_attachment([Db, Id, Attachment]) ->
    find_attachment([Db, Id, Attachment, []]);
find_attachment([Db = ?MEDIA_DB, Id, Attachment, Options])
  when is_list(Options) ->
    {'ok', #media_store_path{db = Db
                            ,id = Id
                            ,att = Attachment
                            ,opt = Options
                            }
    };
find_attachment([?MATCH_ACCOUNT_RAW(Account), Id, Attachment, Options]) ->
    AccountDb =  kzs_util:format_account_db(Account),
    {'ok', #media_store_path{db = AccountDb
                            ,id = Id
                            ,att = Attachment
                            ,opt = Options
                            }
    };
find_attachment([?MATCH_ACCOUNT_UNENCODED(Account), Id, Attachment, Options]) ->
    AccountDb =  kzs_util:format_account_db(Account),
    {'ok', #media_store_path{db = AccountDb
                            ,id = Id
                            ,att = Attachment
                            ,opt = Options
                            }
    };
find_attachment([Db, Id, Attachment, Options]) ->
    {'ok', #media_store_path{db = Db
                            ,id = Id
                            ,att = Attachment
                            ,opt = Options
                            }
    }.

-spec maybe_find_attachment(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}} |
          {'error', 'not_found' | 'no_data'}.
maybe_find_attachment(?MEDIA_DB = Db, Id) ->
    maybe_find_attachment_in_db(Db, Id);
maybe_find_attachment(Db, Id) ->
    AccountDb = kzs_util:format_account_db(Db),
    maybe_find_attachment_in_db(AccountDb, Id).

-spec maybe_find_attachment_in_db(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}} |
          {'error', 'not_found' | 'no_data'}.
maybe_find_attachment_in_db(Db, Id) ->
    case kz_datamgr:open_cache_doc(Db, Id, [{'cache_failures', ['not_found']}]) of
        {'error', _R} ->
            lager:debug("unable to open media doc ~s in ~s: ~p", [Id, Db, _R]),
            {'error', 'not_found'};
        {'ok', JObj} ->
            maybe_find_attachment(Db, Id, JObj)
    end.

-spec maybe_find_attachment(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object()) ->
          {'ok', {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()}} |
          {'error', 'not_found' | 'no_data'}.
maybe_find_attachment(Db, Id, JObj) ->
    lager:debug("trying to find first attachment on doc ~s in db ~s", [Id, Db]),
    case kz_doc:attachment_names(JObj) of
        [] ->
            lager:debug("media doc ~s in ~s has no attachments", [Id, Db]),
            {'error', 'no_data'};
        [AttachmentName | _] ->
            AccountId = kzs_util:format_account_id(Db),
            lager:debug("found first attachment ~s on ~s in ~s", [AttachmentName, Id, Db]),
            find_attachment([AccountId, Id, AttachmentName, [{'doc_type', kz_doc:type(JObj)}
                                                            ,{'rev', kz_doc:revision(JObj)}
                                                            ]])
    end.
