%%%-------------------------------------------------------------------
%%% @author Kamyar Navidan <kamyar@arrakis>
%%% @copyright (C) 2015, Kamyar Navidan
%%% @doc
%%%
%%% @end
%%% Created : 11 Feb 2015 by Kamyar Navidan <kamyar@arrakis>
%%%-------------------------------------------------------------------
-module(wotan_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    WotanManagerSup = {wotan_manager_sup, {wotan_manager_sup, start_link, []},
		       permanent, brutal_kill, supervisor, [wotan_manager_sup]},
    WotanWorkerSup = {wotan_worker_sup, {wotan_worker_sup, start_link, []},
		      permanent, brutal_kill, supervisor, [wotan_worker_sup]},

    {ok, {SupFlags, [WotanManagerSup, WotanWorkerSup]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
