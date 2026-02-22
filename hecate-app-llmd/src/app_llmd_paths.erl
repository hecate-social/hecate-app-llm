-module(app_llmd_paths).

-export([base_dir/0, sqlite_dir/0, sqlite_path/1, reckon_dir/0, reckon_path/1,
         socket_dir/0, socket_path/1, run_dir/0, run_path/1,
         connectors_dir/0, ensure_layout/0, providers_file/0]).

base_dir() ->
    case application:get_env(hecate_app_llmd, data_dir) of
        {ok, Dir} -> expand_path(Dir);
        undefined -> expand_path("~/.hecate/hecate-app-llmd")
    end.

sqlite_dir()     -> filename:join(base_dir(), "sqlite").
sqlite_path(N)   -> filename:join(sqlite_dir(), N).
reckon_dir()     -> filename:join(base_dir(), "reckon-db").
reckon_path(N)   -> filename:join(reckon_dir(), N).
socket_dir()     -> filename:join(base_dir(), "sockets").
socket_path(N)   -> filename:join(socket_dir(), N).
run_dir()        -> filename:join(base_dir(), "run").
run_path(N)      -> filename:join(run_dir(), N).
connectors_dir() -> filename:join(base_dir(), "connectors").

providers_file() -> filename:join(base_dir(), "providers.json").

ensure_layout() ->
    Dirs = [sqlite_dir(), reckon_dir(), socket_dir(), run_dir(), connectors_dir()],
    lists:foreach(fun(Dir) -> ok = filelib:ensure_path(Dir) end, Dirs).

expand_path("~/" ++ Rest) -> filename:join(os:getenv("HOME"), Rest);
expand_path(Path)         -> Path.
