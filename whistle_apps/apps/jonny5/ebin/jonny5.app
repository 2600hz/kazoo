{application,jonny5,
             [{description,"Jonny 5 - Short circuit accounts exceeding call volume"},
              {vsn,"0.0.1"},
              {modules,[j5_acctmgr,jonny5,jonny5_acct,jonny5_acct_sup,
                        jonny5_app,jonny5_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,crypto]},
              {mod,{jonny5_app,[]}},
              {env,[]}]}.
