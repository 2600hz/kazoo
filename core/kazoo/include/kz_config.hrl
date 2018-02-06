-ifndef(KZ_CONFIG_HRL).

-define(FEDERATE_BINDING(App)
       ,case kapps_config:get_is_true(App, <<"use_federated_listener">>, 'false') of
            'true' -> ['federate'];
            _ -> []
        end
       ).

-define(KZ_CONFIG_HRL, 'true').
-endif.
