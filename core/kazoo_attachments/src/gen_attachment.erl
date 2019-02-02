%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2018-2019, 2600Hz
%%% @doc Storage attachment behaviour.
%%% @author Harenson Henao
%%% @end
%%%-----------------------------------------------------------------------------
-module(gen_attachment).

-include("kz_att.hrl").

-type settings()         :: map(). % map of the "settings" object from handler's config
-type db_name()          :: kz_term:ne_binary().
-type doc_id()           :: kz_term:ne_binary().
-type att_name()         :: kz_term:ne_binary().
-type contents()         :: binary().
-type options()          :: kz_data:options().
-type handler_props()    :: kz_data:connection().

-type put_response()     :: {'ok', [{atom(), [{binary(), binary() | kz_json:object()}]}]} |
                            kz_att_error:error().

-type fetch_response()   :: {'ok', iodata()} |
                            kz_att_error:error().

-export_type([settings/0
             ,db_name/0
             ,doc_id/0
             ,att_name/0
             ,contents/0
             ,options/0
             ,handler_props/0
             ,put_response/0
             ,fetch_response/0
             ]).

%%%=============================================================================
%%% Callbacks
%%%=============================================================================

-callback put_attachment(settings()
                        ,db_name()
                        ,doc_id()
                        ,att_name()
                        ,contents()
                        ,options()
                        ) -> put_response().

-callback fetch_attachment(handler_props()
                          ,db_name()
                          ,doc_id()
                          ,att_name()
                          ) -> fetch_response().
