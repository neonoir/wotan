-module(wotan_worker).

-export([start_link/0]).

-include("wotan.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

start_link() ->
    Pid = spawn(fun() -> init()	end),
    {ok, Pid}.

init() ->
    Channel = wotan_rmq_utils:get_channel("localhost"),
    TaskQueue = wotan_rmq_utils:worker_queue(Channel, <<"task_queue">>),
    wotan_rmq_utils:subscribe(Channel, TaskQueue),
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Msg}} ->
	    {TaskMsg, NewTaskMsg} = do_task(Msg),
	    wotan_manager:assign_task(Channel, term_to_binary(NewTaskMsg)),
	    TaskMsg1 = TaskMsg#taskmsg{status = done},
	    
	    %% send reslut to manager <-----
	    wotan_rmq_utils:ack(Channel, Tag),
            loop(Channel)
    end.
    
do_task(Msg) ->    
    TaskMsg = #taskmsg{
		 taskmsg_id = #taskmsg_id{
				 job_id = JobId, 
				 task_id = TaskId},
		 task = #task{
			   module = M, 
			   function = F, 
			   arguments = A},
		 status = assigned} = binary_to_term(Msg),
    NewTask = #task{} = apply(M, F, A),
    NewTaskId = uuid:get_v4(),
    NewTaskMsg = #taskmsg{
		    taskmsg_id = #taskmsg_id{
				    job_id = JobId,
				    task_id = NewTaskId},
		    task = NewTask,
		    status = assigned},			    
    {TaskMsg, NewTaskMsg}.
   
    
    

