-ifndef(KAZOO_SERVICES_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_services/include/kazoo_services.hrl").

-define(APP, 'kazoo_services').
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).

-define(CONFIG_CAT, <<"services">>).

-define(CACHE_NAME, 'kazoo_services_cache').

-define(DEFAULT_QUANTIFIERS, [{<<"users">>
                              ,[<<"user">>
                               ,<<"admin">>
                               ]
                              }
                             ,{<<"devices">>
                              ,[<<"sip_device">>
                               ,<<"ata">>
                               ,<<"cellphone">>
                               ,<<"fax">>
                               ,<<"landline">>
                               ,<<"mobile">>
                               ,<<"sip_uri">>
                               ,<<"smartphone">>
                               ,<<"softphone">>
                               ,<<"ctu">>
                               ,<<"application">>
                               ]
                              }
                             ,{<<"limits">>
                              ,[<<"twoway_trunks">>
                               ,<<"inbound_trunks">>
                               ,<<"outbound_trunks">>
                               ]
                              }
                             ,{<<"branding">>
                              ,[<<"whitelabel">>]
                              }
                             ,{<<"ips">>
                              ,[<<"dedicated">>]
                              }
                             ,{<<"phone_numbers">>
                              ,[<<"caribbean">>
                               ,<<"did_us">>
                               ,<<"emergency">>
                               ,<<"international">>
                               ,<<"toll_us">>
                               ,<<"tollfree_us">>
                               ,<<"unknown">>
                               ]
                              }
                             ,{<<"number_carriers">>
                              ,[<<"knm_bandwidth2">>
                               ,<<"knm_bandwidth">>
                               ,<<"knm_inum">>
                               ,<<"knm_inventory">>
                               ,<<"knm_local">>
                               ,<<"knm_managed">>
                               ,<<"knm_mdn">>
                               ,<<"knm_other">>
                               ,<<"knm_reserved">>
                               ,<<"knm_reserved_reseller">>
                               ,<<"knm_simwood">>
                               ,<<"knm_telnyx">>
                               ,<<"knm_vitelity">>
                               ,<<"knm_voip_innovations">>
                               ,<<"knm_voxbone">>
                               ]
                              }
                             ,{<<"number_services">>
                              ,[<<"cnam">>
                               ,<<"e911">>
                               ,<<"port">>
                               ,<<"sms">>
                               ,<<"mms">>
                               ]
                              }
                             ,{<<"port_request">>
                              ,[<<"canceled">>
                               ,<<"completed">>
                               ,<<"first_canceled">>
                               ,<<"first_completed">>
                               ,<<"first_pending">>
                               ,<<"first_rejected">>
                               ,<<"first_scheduled">>
                               ,<<"first_submitted">>
                               ,<<"first_unconfirmed">>
                               ,<<"pending">>
                               ,<<"rejected">>
                               ,<<"scheduled">>
                               ,<<"submitted">>
                               ,<<"unconfirmed">>
                               ]
                              }
                             ,{<<"qubicle">>
                              ,[<<"basic_queue">>
                               ,<<"pro_queue">>
                               ,<<"basic_recipient">>
                               ,<<"pro_recipient">>
                               ]
                              }
                             ,{<<"voicemails">>
                              ,[<<"mailbox">>
                               ,<<"transcription">>
                               ]
                              }
                             ,{<<"faxes">>
                              ,[<<"mailbox">>]
                              }
                             ,{<<"conferences">>
                              ,[<<"conference">>]
                              }
                             ]).

-define(KAZOO_SERVICES_HRL, 'true').
-endif.
