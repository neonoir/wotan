-module(wotan_manager).

-export([start_link/1]).

-export([assign_tasks/4]).

-include("wotan.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

start_link(Db) ->
    Pid= spawn(fun() -> init(Db) end),
    {ok, Pid}.

init(Db) ->
    Channel = wotan_rmq_utils:get_channel("localhost"),
    wotan_rmq_utils:declare_worker_queue(Channel, <<"task_queue">>),
    JobQueue = wotan_rmq_utils:declare_worker_queue(Channel, <<"job_queue">>),
    wotan_rmq_utils:subscribe(Channel, JobQueue),
    loop(Channel, Db).

loop(Channel, Db) ->
    receive
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Msg}} ->
	    Job = #job{job_id = JobId, tasks = Tasks} = binary_to_term(Msg),
	    wotan:log("* job received by manager: ~p~n", [Job]),
	    assign_tasks(Channel, Db, JobId, Tasks),
	    wotan_rmq_utils:ack(Channel, Tag),
	    loop(Channel, Db);
	xcv ->
	    io:format("XCV")
    end.

assign_tasks(_, _, _, []) ->
    ok;
assign_tasks(Channel, Db, JobId, [Task|Tasks]) ->
    TaskId = list_to_binary(uuid:uuid_to_string(uuid:get_v4(), nodash)),
    TaskMsg = #taskmsg{
	     taskmsg_id = #taskmsg_id{
			     job_id = JobId, 
			     task_id = TaskId}, 
	     task = Task,
	     status = <<"assigned">>},
    
    assign_task(Channel, Db, TaskMsg),
    assign_tasks(Channel, Db, JobId, Tasks).

assign_task(Channel, Db, TaskMsg) ->
    wotan_couch:save_task_status(Db, TaskMsg),
    wotan:log("* task assigned: ~p~n", [TaskMsg]),
    amqp_channel:cast(Channel,
                      #'basic.publish'{
			 exchange = <<"">>,
			 routing_key = <<"task_queue">>},
                      #amqp_msg{
			 props = #'P_basic'{delivery_mode = 2},
			 payload = term_to_binary(TaskMsg)}).
