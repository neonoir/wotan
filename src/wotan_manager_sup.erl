%%%-------------------------------------------------------------------
%%% @author Kamyar Navidan <kamyar@arrakis>
%%% @copyright (C) 2015, Kamyar Navidan
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2015 by Kamyar Navidan <kamyar@arrakis>
%%%-------------------------------------------------------------------
-module(wotan_manager_sup).

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

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Db) ->
    supervisor:start_child(?SERVER, [Db]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = permanent,
    Shutdown = brutal_kill,
    Type = worker,

    AChild = {wotan_manager, {wotan_manager, start_link, []},
	      Restart, Shutdown, Type, [wotan_manager]},

    {ok, {SupFlags, [AChild]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



