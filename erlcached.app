{application, erlcached,
  [{description, "Erlcached: Memcached in Erlang OTP"},
   {vsn, "0.1"},
   {modules, [erlcached_app, erlcached_supervisor, erlcached_server]},
   {registered, [erlcached_server, erlcached_super]},
   {applications, [kernel, stdlib]},
   {mod, {erlcached_app, []}},
   {start_phases, []}
  ]}.
