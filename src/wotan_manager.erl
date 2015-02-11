-module(wotan_manager).

-export([init/0,
	 assign_tasks/5]).

-include("wotan.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").


init() ->
    Channel = wotan_rmq_utils:get_channel("localhost"),
    wotan_rmq_utils:declare_worker_queue(Channel, <<"task_queue">>),
    JobQueue = wotan_rmq_utils:declare_worker_queue(Channel, <<"job_queue">>),
    wotan_rmq_utils:subscribe(Channel, JobQueue),
    loop(Channel).

loop(Channel) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Msg}} ->
	    #job{id = JobId, tasks = Tasks} = binary_to_term(Msg),
	    spawn(?MODULE, assign_tasks, [Channel, Tag, self(), JobId, Tasks]),
	    loop(Channel);
	{job_assigned, Tag} ->
	    %% log it
	    wotan_rmq_utils:ack(Channel, Tag),
	    loop(Channel)
    end.

assign_tasks(_, Tag, Manager, _, []) ->
    Manager ! {job_assigned, Tag};
assign_tasks(Channel, Tag, Manager, JobId, [Task|Tasks]) ->
    TaskId = uuid:get_v4(),
    Msg = #taskmsg{
	     taskmsg_id = #taskmsg_id{job_id = JobId, task_id = TaskId}, 
	     task = Task,
	     status = assigned},
    assign_task(Channel, Msg),
    assign_tasks(Channel, Tag, Manager, JobId, Tasks).

assign_task(Channel, Msg) ->
    amqp_channel:cast(Channel,
                      #'basic.publish'{
			 exchange = <<"">>,
			 routing_key = <<"task_queue">>},
                      #amqp_msg{
			 props = #'P_basic'{delivery_mode = 2},
			 payload = term_to_binary(Msg)}).
