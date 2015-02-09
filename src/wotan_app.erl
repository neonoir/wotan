%%%-------------------------------------------------------------------
%%% @author Kamyar Navidan <kamyar.n@gmail.com>
%%% @copyright (C) 2015, Kamyar Navidan
%%% @doc
%%%
%%% @end
%%% Created :  8 Feb 2015 by Kamyar Navidan <kamyar.n@gmail.com>
%%%-------------------------------------------------------------------
-module(wotan_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
start(_StartType, _StartArgs) ->
    wotan_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
