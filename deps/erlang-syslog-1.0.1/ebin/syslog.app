{application,syslog,
             [{description,"Syslog for erlang"},
              {vsn,"1.0.1"},
              {registered,[syslog_sup,syslog]},
              {applications,[kernel,stdlib]},
              {mod,{syslog_app,[]}},
              {env,[]},
              {modules,[syslog,syslog_app,syslog_sup]}]}.
