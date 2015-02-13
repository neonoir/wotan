-include_lib("amqp_client/include/amqp_client.hrl").

-record(task, {
	  mod :: module(),
	  func :: function(),
	  args :: [any()]}).
-type task() :: #task{}.
-record(taskmsg_id, {job_id :: binary(), 
		     task_id :: binary()}).
-type taskmsg_id() :: #taskmsg_id{}.
-record(taskmsg, {taskmsg_id :: taskmsg_id(), 
		  task :: task(),
		  status :: assigned | done | error}).
-type taskmsg() :: #taskmsg{}.
-record(job, {
	  job_id :: binary(),
	  tasks :: [task()]}).
-type job() :: #job{}.
