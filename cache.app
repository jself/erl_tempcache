{application, cache,
    [{description, "Local cache server"},
     {vsn, "0.1"},
     {modules, [cache_app, cache, cache_free, cache_sup, cache_server_sup, cache_free_sup]},
     {registered, [cache, cache_sup, cache_server_sup, cache_free_sup]},
     {applications, [kernel, stdlib]},
     {mod, {cache_app, []}}
 ]}.
