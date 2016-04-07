-ifndef(KNM_RECORDS_HRL).

-record(knm_number, {knm_phone_number :: knm_phone_number:knm_phone_number()
                     ,services :: wh_services:services()
                     ,billing_id :: api_binary()
                     ,transactions = [] :: wh_transaction:transactions()
                     ,errors = [] :: list()
                     ,charges = [] :: [{ne_binary(), integer()}]
                    }).

-record(knm_phone_number, {number :: ne_binary()
                           ,number_db :: ne_binary()
                           ,assign_to :: api_binary()
                           ,assigned_to :: api_binary()
                           ,prev_assigned_to :: api_binary()
                           ,used_by :: api_binary()
                           ,features = wh_json:new() :: wh_json:object()
                           ,state :: ne_binary()
                           ,reserve_history = [] :: ne_binaries()
                           ,ported_in = 'false' :: boolean()
                           ,module_name :: ne_binary()
                           ,carrier_data :: wh_json:object()
                           ,region :: ne_binary()
                           ,auth_by :: api_binary()
                           ,dry_run = 'false' :: boolean()
                           ,locality :: wh_json:object()
                           ,doc :: wh_json:object()
                          }).

-define(KNM_RECORDS_HRL, 'true').
-endif.
