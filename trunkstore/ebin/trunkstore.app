{application,trunkstore,
             [{description,"Trunk Store Backend - Authentication and Routing"},
              {vsn,"0.1"},
              {registered,[]},
              {applications,[kernel,stdlib,amqp]},
              {mod,{trunkstore_app,[]}},
              {env,[]},
              {modules,[logger,trunkstore_app,trunkstore_sup,ts_auth,
                        ts_responder,ts_route,whistle_api]}]}.
