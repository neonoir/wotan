-record(task, {
	  module :: module(),
	  function :: function(),
	  arguments :: [any()]}).
-type task() :: #task{}.
-record(taskmsg_id, {job_id :: atom(), 
		     task_id :: atom()}).
-type taskmsg_id() :: #taskmsg_id{}.
-record(taskmsg, {taskmsg_id :: taskmsg_id(), 
		  task :: task(),
		  status :: assigned | done | error}).
-type taskmsg() :: #taskmsg{}.
-record(job, {
	  id :: atom(),
	  tasks :: [task()]}).
-type job() :: #job{}.
