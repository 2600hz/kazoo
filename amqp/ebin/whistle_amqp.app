{application,whistle_amqp,
             [{description,"AMQP client helpers for interacting with a AMQP server"},
              {vsn,"0.5.1"},
              {modules,[amqp_manager,amqp_util,logger,props,whistle_amqp,
                        whistle_amqp_app,whistle_amqp_deps,whistle_amqp_sup]},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{whistle_amqp_app,[]}},
              {env,[]}]}.
