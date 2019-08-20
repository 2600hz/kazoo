%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc Handle CRUD operations for Directories
%%% @author James Aimonetti
%%% @author Luis Azedo
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_freeswitch).

-export([init/0
        ,allowed_methods/0
        ,resource_exists/0
        ,validate_freeswitch/1
        ,content_types_provided/1
        ,authenticate/1
        ,authorize/1
        ]).

-include("crossbar.hrl").

-define(FS_OFFLINE_SERVER, 'crossbar_freeswitch').

-define(MIME_TYPE_GZIP, {<<"application">>, <<"x-gzip">>}).
-define(MIME_TYPE_ZIP, {<<"application">>, <<"zip">>}).
-define(MIME_TYPE_ZIP2, <<"application/zip">>).
-define(MIME_TYPE_RAR, {<<"application">>, <<"x-rar-compressed">>}).
-define(MIME_TYPE_TAR, {<<"application">>, <<"x-tar">>}).

-define(MIME_TYPES, [?MIME_TYPE_GZIP
                    ,?MIME_TYPE_ZIP
                    ,?MIME_TYPE_RAR
                    ,?MIME_TYPE_TAR
                    ]).

-define(MOD_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".freeswitch">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> ok.
init() ->
    _ = supervisor:start_child('crossbar_sup', ?WORKER(?FS_OFFLINE_SERVER)),
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.freeswitch">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.freeswitch">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.freeswitch">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.freeswitch">>, ?MODULE, 'validate_freeswitch'),
    ok.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    authenticate(cb_context:req_nouns(Context), cb_context:req_verb(Context), Context).

-spec authenticate(req_nouns(), http_method(), cb_context:context()) -> boolean().
authenticate([{<<"freeswitch">>,[]}], ?HTTP_GET, Context) ->
    UserKey = kz_json:get_value(<<"key">>, cb_context:query_string(Context)),
    ServerKey = kapps_config:get_binary(?MOD_CONFIG_CAT
                                       ,<<"offline_configuration_key">>
                                       ,kz_binary:rand_hex(32)),
    case UserKey =:= ServerKey of
        'true' ->
            lager:debug("authenticating offline configuration request", []),
            'true';
        'false' ->
            lager:debug("request for offline configuration with invalid key ~s"
                       ,[UserKey]),
            'false'
    end;
authenticate(_Nouns, _Verb, _Context) -> 'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    authorize(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

-spec authorize(req_nouns(), http_method()) -> boolean().
authorize([{<<"freeswitch">>,[]}], ?HTTP_GET) ->
    lager:debug("authorizing offline configuration request", []),
    'true';
authorize(_Nouns, _Verb) -> 'false'.

-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    cb_context:set_content_types_provided(Context, [{'to_binary', ?MIME_TYPES}]).

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------
-spec validate_freeswitch(cb_context:context()) -> cb_context:context().
validate_freeswitch(Context) ->
    validate_freeswitch(Context, cb_context:req_verb(Context)).

-spec validate_freeswitch(cb_context:context(), http_method()) -> cb_context:context().
validate_freeswitch(Context, ?HTTP_GET) ->
    maybe_load_last_data(Context).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_load_last_data(cb_context:context()) -> cb_context:context().
maybe_load_last_data(Context) ->
    case gen_server:call(crossbar_sup:find_proc(?FS_OFFLINE_SERVER), 'current') of
        {'ok', File} -> load_last_data(Context, File);
        {'error', Error} ->
            cb_context:add_system_error(Error, Context)
    end.

-spec load_last_data(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_last_data(Context, File) ->
    {'ok', AttachBin} = file:read_file(File),
    BaseName = kz_term:to_binary(filename:basename(File)),
    ContentType = extension_to_content_type(kz_term:to_lower_binary(filename:extension(BaseName))),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_status/2, 'success'}
                       ,{fun cb_context:set_resp_data/2, AttachBin}
                       ,{fun cb_context:add_resp_headers/2,
                         #{<<"content-disposition">> => <<"attachment; filename=", BaseName/binary>>
                          ,<<"content-type">> => ContentType
                          }
                        }
                       ]).

-spec extension_to_content_type(kz_term:ne_binary()) -> kz_term:ne_binary().
extension_to_content_type(<<".gzip">>) -> ?MIME_TYPE_GZIP;
extension_to_content_type(<<".zip">>) -> ?MIME_TYPE_ZIP2;
extension_to_content_type(<<".rar">>) -> ?MIME_TYPE_RAR;
extension_to_content_type(<<".tar">>) -> ?MIME_TYPE_TAR.
