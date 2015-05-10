-module(wotan_tasks).

-export([assign_tasks/4,
	 assign_task/3]).

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
    wotan_rmq_utils:publish(Channel, <<"">>, <<"task_queue">>, TaskMsg).
