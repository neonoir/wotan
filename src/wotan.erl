-module(wotan).




start(N) ->
    ensure_deps_started(),
    ensure_started(wotan),
    start_workers(N).

start_workers(N) ->
    [wotan_worker_sup:start_child() || _ <- lists:seq(1, N)].

-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

-spec ensure_deps_started() -> ok.
ensure_deps_started() ->
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(cowlib),
    ensure_started(cowboy).
