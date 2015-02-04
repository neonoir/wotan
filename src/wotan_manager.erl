-module(wotan_manager).

-record(task, {
	  module :: module(),
	  function :: function(),
	  arguments :: [any()]}).
-type task() :: #task{}.
-record(job, {
	  id :: atom(),
	  tasks = [task()]}).
-type job() :: #job{}.
-record(taskmsg, {
	  task_id :: reference(),
	  job_id :: atom(),
	  task :: task(),
	  status :: assigned | done | error}).
-type taskmsg() :: #taskmsg{}.

init(Job, Host) ->
    Channel = wotan_rmq_utils:get_channel(Host),
    WorkerQueue = wotan_rmq_utils:worker_queue(Channel, <<"wotan_task_queue">>),
    #job{id = JobId, tasks = Tasks} = Job,
    ets:new(JobId, [named_table]),
    ok = assign_tasks(Channel, JobId, Tasks).
    


assign_tasks(Channel, JobId, []) ->
    ok;
assign_tasks(Channel, JobId, [Task|Tasks]) ->
    TaskId = make_ref(),
    Msg = #taskmsg{task_is = TaskId, 
		   job_id = JobId, 
		   task = Task,
		   status = assigned},
    wotan_rmq_utils:assign_task(Channel, term_to_binary(Msg)),
    ets:insert(JobId, Msg),
    assign_task(Channel, JobId, Tasks).
