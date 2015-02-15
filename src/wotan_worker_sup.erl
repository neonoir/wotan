%%%-------------------------------------------------------------------
%%% @author Kamyar Navidan <kamyar.n@gmail.com>
%%% @copyright (C) 2015, Kamyar Navidan
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2015 by Kamyar Navidan <kamyar.n@gmail.com>
%%%-------------------------------------------------------------------
-module(wotan_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
	 start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Db) ->
    supervisor:start_child(?SERVER, [Db]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,

    WorkerChild = {wotan_worker, {wotan_worker, start_link, []},
		   Restart, Shutdown, Type, [wotan_wroker]},

    {ok, {SupFlags, [WorkerChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
