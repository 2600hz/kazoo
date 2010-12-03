{application,hangups,
             [{description,"Listen for abnormal hangups and store them in Couch"},
              {vsn,"0.1.0"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{hangups_app,[]}},
              {env,[]},
              {modules,[hangups,hangups_app,hangups_deps,hangups_listener,
                        hangups_sup]}]}.
