{application,dth,
             [{description,"DTH - Integration with DTH Billing System"},
              {vsn,"0.1.0"},
              {modules,[dth,dth_app,dth_cdr_listener,dth_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,crypto]},
              {mod,{dth_app,[]}},
              {env,[]}]}.
