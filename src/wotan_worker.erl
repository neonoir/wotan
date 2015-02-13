-module(wotan_worker).

-export([start_link/0]).

-include("wotan.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

start_link() ->
    Pid = spawn(fun() -> init()	end),
    {ok, Pid}.

init() ->
    Channel = wotan_rmq_utils:get_channel("localhost"),
    TaskQueue = wotan_rmq_utils:declare_worker_queue(Channel, <<"task_queue">>),
    wotan_rmq_utils:subscribe(Channel, TaskQueue),
    wotan:log("* worker ~p started. waiting for messages.~n", [self()]),
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Msg}} ->
	    #taskmsg{
	       taskmsg_id = #taskmsg_id{job_id = JobId},
	       task = #task{mod = M, 
			    func = F, 
			    args = A},
	       status = assigned} = binary_to_term(Msg),
	    wotan:log("* ~p received ~p.~n", [self(), {M, F, A}]),
	    NewTasks = apply(M, F, A),
	    case NewTasks of
		[] -> 
		    wotan_rmq_utils:ack(Channel, Tag),
		    loop(Channel);
		_ ->
		    wotan_manager:assign_tasks(Channel, JobId, NewTasks),
		    %% TaskMsg1 = TaskMsg#taskmsg{status = done},
		    %% log TaskMsg1
		    wotan_rmq_utils:ack(Channel, Tag),
		    loop(Channel)
	    end	 
    end.  
    
    

