-module(wotan_couch).

-export([server/1,
	 db/0,
	 db/2,
	 save_task_status/2]).

-include("wotan.hrl").

server(Url) ->
    couchbeam:server_connection(Url).

db(Server, DbName) ->
    {ok, Db} = couchbeam:open_db(Server, DbName),
    Db.

db() ->
    db(server("http://localhost:5984"), "wotan_task_status").

save_task_status(Db, TaskMsg) ->
    #taskmsg{
       taskmsg_id = #taskmsg_id{job_id = JobId, task_id = TaskId},
       task = #task{mod = M, func = F, args = A},
       status = Status} = TaskMsg,
    Doc = {[
	    {<<"_id">>, doc_id()},
	    {<<"job_id">>, JobId},
	    {<<"task_id">>, TaskId},
	    {<<"status">>, Status},
	    {<<"task">>, format_mfa([M, F, A])},
	    {<<"type">>, <<"task_status">>}]},
    wotan:log("save task status: ~p~n~n.", [Doc]),
    {ok, _} = couchbeam:save_doc(Db, Doc).

format_mfa(MFA) ->
    list_to_binary(
      lists:flatten(
	io_lib:format("[~p, ~p, ~p]", MFA))).

doc_id() ->    
    list_to_binary(uuid:uuid_to_string(uuid:get_v4(), nodash)).




