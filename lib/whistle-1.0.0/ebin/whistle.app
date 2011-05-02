{application,whistle,
             [{description,"Whistle Helpers"},
              {vsn,"0.2.0"},
              {registered,[]},
              {applications,[kernel,stdlib,crypto,sasl]},
              {mod,{whistle_app,[]}},
              {env,[]},
              {modules,[logger,props,wh_cache,wh_json,wh_timer,whistle_api,
                        whistle_util]}]}.
