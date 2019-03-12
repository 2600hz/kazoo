%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(pqc_cb_whitelabel).

%% Manual testing
-export([create_whitelabel/2
        ]).

-include("kazoo_proper.hrl").


-spec create_whitelabel(pqc_cb_api:state(), kz_doc:setter_funs()) -> pqc_cb_api:response().
create_whitelabel(API, Setters) ->
    Envelope = pqc_cb_api:create_envelope(kz_doc:setters(kz_json:new(), Setters)),
    pqc_cb_api:make_request([201]
                           ,fun kz_http:put/3
                           ,whitelabel_url(pqc_cb_api:auth_account_id(API))
                           ,pqc_cb_api:request_headers(API)
                           ,kz_json:encode(Envelope)
                           ).

-spec whitelabel_url(string() | kz_term:ne_binary()) -> string().
whitelabel_url(AccountId) ->
    string:join([pqc_cb_accounts:account_url(AccountId), "whitelabel"], "/").
