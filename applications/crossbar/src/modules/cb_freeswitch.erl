%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% Handle CRUD operations for Directories
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Luis Azedo
%%%-------------------------------------------------------------------
-module(cb_freeswitch).

-export([init/0
         ,allowed_methods/0
         ,resource_exists/0
         ,validate_freeswitch/1
         ,content_types_provided/1
         ,authenticate/1
         ,authorize/1
        ]).

-export([freeswitch_periodic_build/0]).

-include("../crossbar.hrl").

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

%%%===================================================================
%%% API
%%%===================================================================
init() ->
    _ = crossbar_bindings:bind(<<"*.authenticate">>, ?MODULE, 'authenticate'),
    _ = crossbar_bindings:bind(<<"*.authorize">>, ?MODULE, 'authorize'),
    _ = crossbar_bindings:bind(<<"*.content_types_provided.freeswitch">>, ?MODULE, 'content_types_provided'),
    _ = crossbar_bindings:bind(<<"*.allowed_methods.freeswitch">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.freeswitch">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.freeswitch">>, ?MODULE, 'validate_freeswitch'),
    _ = crossbar_bindings:bind(crossbar_cleanup:binding_minute(), ?MODULE, 'freeswitch_periodic_build').


-spec authorize(cb_context:context()) -> boolean().
authorize(Context) ->
    lager:debug("CB_FREESWITCH AUTHORIZE"),
    authorize(cb_context:req_nouns(Context), cb_context:req_verb(Context), Context).

authorize([{<<"freeswitch">>,[]}], ?HTTP_GET, Context) ->
    UserKey = wh_json:get_value(<<"key">>, cb_context:query_string(Context)),
    ServerKey = whapps_config:get_binary(?MOD_CONFIG_CAT
                                         ,<<"offline_configuration_key">>
                                         ,wh_util:rand_hex_binary(32)),
    lager:debug("UserKey =:= ServerKey => ~p",[UserKey =:= ServerKey]),
    UserKey =:= ServerKey;
authorize(_Nouns, _Verb, Context) ->
    lager:debug("CB_FREESWITCH NOUNS ~p",[_Nouns]),
    'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec authenticate(cb_context:context()) -> boolean().
authenticate(Context) ->
    lager:debug("CB_FREESWITCH AUTHENTICATE"),
    authenticate(cb_context:req_nouns(Context), cb_context:req_verb(Context)).

authenticate([{<<"freeswitch">>,[]}], ?HTTP_GET) ->    
    'true';
authenticate(_Nouns, _Verb) ->
    lager:debug("CB_FREESWITCH NOUNS ~p",[_Nouns]),
    'false'.


-spec content_types_provided(cb_context:context()) -> cb_context:context().
content_types_provided(Context) ->
    lager:debug("CB_FREESWITCH CNTENT-TYPE"),
    cb_context:set_content_types_provided(Context, [{'to_binary', ?MIME_TYPES}]).


%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() ->
    true.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate_freeswitch(cb_context:context()) -> cb_context:context().
validate_freeswitch(#cb_context{req_verb = ?HTTP_GET}=Context) ->
    maybe_load_last_data(Context).


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec maybe_load_last_data(cb_context:context()) -> cb_context:context().
maybe_load_last_data(Context) ->
    case gen_server:call(crossbar_sup:find_proc(?FS_OFFLINE_SERVER), 'current') of
        {'ok', File} ->
            lager:debug("CB_FREESWITCH ~s",[File]),
            load_last_data(Context, File);
        {'error', Error} ->
            cb_context:add_system_error(Error, Context)
    end.


-spec load_last_data(cb_context:context(), ne_binary()) -> cb_context:context().
load_last_data(Context, File) ->
    {'ok', AttachBin} = file:read_file(File),
    BaseName = wh_util:to_binary(filename:basename(File)),
    cb_context:setters(
      Context,[{fun cb_context:set_resp_status/2, 'success'}
               ,{fun cb_context:set_resp_data/2, AttachBin}
               ,{fun cb_context:add_resp_headers/2,
                  [{<<"Content-Disposition">>, <<"attachment; filename=", BaseName/binary>>}
                   ,{<<"Content-Type">>, extension_to_content_type(
                       wh_util:to_lower_binary(
                         filename:extension(BaseName)))
                    }
                   ,{<<"Content-Length">>, byte_size(AttachBin)}
                  ]}
              ]).
                  
-spec extension_to_content_type(ne_binary()) -> ne_binary().
extension_to_content_type(<<".gzip">>) -> ?MIME_TYPE_GZIP;
extension_to_content_type(<<".zip">>) -> ?MIME_TYPE_ZIP2;
extension_to_content_type(<<".rar">>) -> ?MIME_TYPE_RAR;
extension_to_content_type(<<".tar">>) -> ?MIME_TYPE_TAR.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempt to load a summarized listing of all instances of this
%% resource.
%% @end
%%--------------------------------------------------------------------
-spec summary(cb_context:context()) -> cb_context:context().
summary(Context) ->
    'ok'.

-spec freeswitch_periodic_build() -> any().
freeswitch_periodic_build() ->
    lager:debug("STARTING OFFLINE FREESWITCH CONFIGURATION"),
    gen_server:cast(crossbar_sup:find_proc(?FS_OFFLINE_SERVER), 'periodic_build').


    

