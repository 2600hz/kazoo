{application,rscrpt,
             [{description,"freeSWITCH Resource Reporter"},
              {vsn,"0.3"},
              {registered,[]},
              {applications,[kernel,stdlib,os_mon,amqp]},
              {modules,[logger,rscrpt,rscrpt_app,rscrpt_deps,rscrpt_fsbox,
                        rscrpt_fsevt,rscrpt_reporter,rscrpt_sup]},
              {mod,{rscrpt_app,[]}},
              {env,[]}]}.
