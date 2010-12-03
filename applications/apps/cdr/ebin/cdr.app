{application,cdr,
             [{description,"Listen and record CDR events into Couch"},
              {vsn,"0.3.0"},
              {registered,[]},
              {applications,[kernel,stdlib]},
              {mod,{cdr_app,[]}},
              {env,[]},
              {modules,[cdr,cdr_app,cdr_deps,cdr_listener,cdr_sup]}]}.
