-module(wotan).

-export([test/1,
	 start/0,
	 assign_job/1,
	 log/2]).

-include("wotan.hrl").

start() ->
    ensure_deps_started(),
    ensure_started(wotan),
    leptus:start_listener(http, [{'_', [{wotan_rest, undefined_state}]}]).

add_workers(N) ->
    [wotan_worker_sup:start_child() || _ <- lists:seq(1, N)],
    log("* ~p workers started.~n", [N]).

assign_job(Job) ->
    Channel = wotan_rmq_utils:get_channel("localhost"),
    wotan_rmq_utils:declare_worker_queue(Channel, <<"job_queue">>),
    amqp_channel:cast(Channel,
                      #'basic.publish'{
			 exchange = <<"">>,
			 routing_key = <<"job_queue">>},
                      #amqp_msg{
			 props = #'P_basic'{delivery_mode = 2},
			 payload = term_to_binary(Job)}),
    log("* job assigned: ~p~n", [Job]).


ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

ensure_deps_started() ->
    ensure_started(crypto),
    ensure_started(ranch),
    ensure_started(cowlib),
    ensure_started(cowboy),
    ensure_started(leptus).

log(P, S) ->
    io:format(
      lists:flatten(
	io_lib:format(P, S))).

test(N) ->
    start(),
    add_workers(N),
    Job = #job{job_id=uuid:get_v4(), tasks=[#task{mod=test_job, func=fun1, args=[]}]},
    assign_job(Job).
