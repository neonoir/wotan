-module(wotan_worker).

-export([start_link/1]).

-include("wotan.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

start_link(Db) ->
    Pid = spawn(fun() -> init(Db) end),
    {ok, Pid}.

init(Db) ->
    Channel = wotan_rmq_utils:get_channel("localhost"),
    TaskQueue = wotan_rmq_utils:declare_worker_queue(Channel, <<"task_queue">>),
    wotan_rmq_utils:subscribe(Channel, TaskQueue),
    wotan:log("* worker ~p started. waiting for messages.~n", [self()]),
    loop(Channel, Db).

loop(Channel, Db) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Msg}} ->
	    TaskMsg = #taskmsg{
			 taskmsg_id = #taskmsg_id{job_id = JobId},
			 task = #task{mod = M, 
				      func = F, 
				      args = A},
			 status = <<"assigned">>} = binary_to_term(Msg),
	    wotan:log("* ~p received ~p.~n", [self(), {M, F, A}]),
	    NewTasks = apply(M, F, A),
	    wotan_couch:save_task_status(Db, TaskMsg#taskmsg{status = <<"done">>}),
	    case NewTasks of
		[] -> 
		    wotan_rmq_utils:ack(Channel, Tag),
		    loop(Channel, Db);
		_ ->
		    wotan_manager:assign_tasks(Channel, Db, JobId, NewTasks),
		    wotan_rmq_utils:ack(Channel, Tag),
		    loop(Channel, Db)
	    end	 
    end.  
    
    

