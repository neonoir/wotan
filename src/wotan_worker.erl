-module(wotan_worker).

-export([init/1]).

-record(task, {module,
	       function,
	       arguments}).

init(Host) ->
    Channel = wotan_rmq_utils:get_channel(Host),
    WorkerQueue = wotan_rmq_utils:worker_queue(Channel, <<"wotan_task_queue">>),
    wotan_rmq_utils:subscribe(Channel, WorkerQueue),
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Msg}} ->
	    {Manager, NewTask} = do_task(Msg),
	    %% send reslut to manager <-----
	    wotan_rmq_utils:ack(Channel, Tag),
            loop(Channel)
    end.
    
do_task(Msg) ->    
    {JobId, TaskId, #task{module = M, 
			  function = F, 
			  arguments = A}} = binary_to_term(Msg),
    NewTask = #task{} = apply(M, F, A),
    {JobId, TaskId, NewTask}.
   
    
    

