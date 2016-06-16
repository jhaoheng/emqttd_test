%% app generated at {2016,6,5} {11,54,24}
{application,os_mon,
             [{description,"CPO  CXC 138 46"},
              {vsn,"2.4"},
              {id,[]},
              {modules,[cpu_sup,disksup,memsup,nteventlog,os_mon,os_mon_mib,
                        os_mon_sysinfo,os_sup]},
              {registered,[os_mon_sup,os_mon_sysinfo,disksup,memsup,cpu_sup,
                           os_sup_server]},
              {applications,[kernel,stdlib,sasl]},
              {included_applications,[]},
              {env,[{start_cpu_sup,true},
                    {start_disksup,true},
                    {start_memsup,true},
                    {start_os_sup,false}]},
              {maxT,infinity},
              {maxP,infinity},
              {mod,{os_mon,[]}}]}.

