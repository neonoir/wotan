-module(test_job).

-export([fun1/0,
	 fun2/0,
	 fun3/1]).

-include("wotan.hrl").

fun1() ->
    [#task{mod = ?MODULE, 
	  func = fun2,
	  args = []},
     #task{mod = ?MODULE, 
	   func = fun2,
	   args = []}].

fun2() ->
    [#task{mod = ?MODULE, 
	  func = fun3,
	  args = [1]},
     #task{mod = ?MODULE, 
	   func = fun3,
	   args = [4]},
     #task{mod = ?MODULE, 
	  func = fun3,
	  args = [asd]},
     #task{mod = ?MODULE, 
	   func = fun3,
	   args = [asd]},
     #task{mod = ?MODULE, 
	   func = fun3,
	   args = [asd]},
     #task{mod = ?MODULE, 
	   func = fun3,
	   args = [asd]}].

fun3(_) ->
    [].
