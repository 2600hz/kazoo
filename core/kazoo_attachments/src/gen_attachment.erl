-module(gen_attachment).

%% Setter
-export([error_response/2]).
%% Getters
-export([error_code/1, error_body/1]).
%% Helper(s)
-export([is_error_response/1]).

-include("kz_att.hrl").

-type settings()         :: kz_data:connection().
-type db_name()          :: kz_term:ne_binary().
-type doc_id()           :: kz_term:ne_binary().
-type att_name()         :: kz_term:ne_binary().
-type contents()         :: kz_term:ne_binary().
-type options()          :: kz_data:options().
-type handler_props()    :: kz_data:connection().
-type error_code()       :: pos_integer() | atom(). % 400, 404, 409, etc.
-type error_body()       :: binary() | % encoded map()
                            bitstring() | % <<"example">>
                            atom(). % 'not_found' | 'return_id_missing' | etc.
-opaque error_response() :: {'gen_attachment_error', [{'error_code', error_code()} |
                                                      {'error_body', error_body()}
                                                     ]}.
-type put_response()     :: {'ok', [{atom(), [{binary(), binary() | kz_json:object()}]}]} |
                            error_response().
-type fetch_response()   :: {'ok', iodata()} | error_response().

-export_type([settings/0
             ,db_name/0
             ,doc_id/0
             ,att_name/0
             ,contents/0
             ,options/0
             ,handler_props/0
             ,error_response/0
             ,put_response/0
             ,fetch_response/0
             ]).

%% Note: Some functions have the `kz_datamgr:data_error()' type in its spec but it is not
%%       being used, it is just to avoid dialyzer complaints because since `cb_storage'
%%       mod is using `kazoo_attachments' app through `kz_datamgr' mod it (cb_storage) can
%%       not get only opaque `error_response()' errors but also `data_error()' from
%%       kz_datamgr and when you call ?MODULE:[error_code/1, error_body/1, is_error_response/1]
%%       you might actually be sending a `data_error()' instead of a `error_response()' value.

%% =======================================================================================
%% Callbacks
%% =======================================================================================
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

%% =======================================================================================
%% API
%% =======================================================================================
%% Setter
-spec error_response(error_code(), error_body()) -> error_response().
error_response(ErrorCode, ErrorBody) ->
    {'gen_attachment_error', [{'error_code', ErrorCode}, {'error_body', ErrorBody}]}.

%% Getters
%% Read the note about `kz_datamgr:data_error()' after the types declaration
-spec error_code(error_response()) -> error_code() | 'undefined'.
error_code({'gen_attachment_error', Reason}) ->
    props:get_value('error_code', Reason).

%% Read the note about `kz_datamgr:data_error()' after the types declaration
-spec error_body(error_response()) -> error_body() | 'undefined'.
error_body({'gen_attachment_error', Reason}) ->
    props:get_value('error_body', Reason).

%% Helper(s)
%% Read the note about `kz_datamgr:data_error()' after the types declaration
-spec is_error_response(error_response() | kz_datamgr:data_error()) -> boolean().
is_error_response({'gen_attachment_error', ErrorProps}) when is_list(ErrorProps) ->
    lists:all(fun check_error_response_element/1, ErrorProps);
is_error_response(_) ->
    'false'.

-spec check_error_response_element(error_code() | error_body()) -> boolean().
check_error_response_element({'error_code', Code}) ->
    is_atom(Code)
        orelse is_integer(Code);
check_error_response_element({'error_body', Body}) ->
    is_binary(Body)
        orelse is_bitstring(Body)
        orelse is_atom(Body);
check_error_response_element(_) ->
    'false'.
