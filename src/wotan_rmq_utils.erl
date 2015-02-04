-module(wotan_rmq_utils).
-compile(export_all).

-include_lib("amqp_client/include/amqp_client.hrl").

-spec get_channel(string()) -> {ok, pid()} | {error, Error}.
get_channel(Host) ->
    case amqp_connection:start(#amqp_params_network{host = Host}) of
	{ok, Connection} ->
	    case amqp_connection:open_channel(Connection) of
		Channel ->
		    Channel;
		{error, Error} ->
		    {error, Error}
	    end;
	{error, Error} ->
	    {error, Error}
    end.

exchange_declare(Channel, Exchange, Type) ->
    amqp_channel:call(Channel, 
		      #'exchange.declare'{exchange = Exchange, type = Type}).

queue_declare(Channel, Method) ->
    #'queue.declare_ok'{queue = Queue} = 
	amqp_channel:call(Channel, Method),
    Queue.

queue_bind(Channel, Exchange, Queue) ->
    amqp_channel:call(Channel, 
		      #'queue.bind'{exchange = Exchange, queue = Queue}).

publish(Channel, Exchange, Payload) ->
    amqp_channel:cast(Channel,
		      #'basic.publish'{exchange = Exchange},
		      #amqp_msg{payload = term_to_binary(Payload)}).

subscribe(Channel, Queue) ->
    #'basic.consume_ok'{consumer_tag = _Tag} = 
	amqp_channel:subscribe(Channel, 
			       #'basic.consume'{queue = Queue}, 
			       self()),
    receive
        #'basic.consume_ok'{} -> ok
    end.

ack(Channel, Tag) ->
    amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}).

worker_queue(Channel, QueueName) ->
    #'queue.declare_ok'{queue = Queue} =
        amqp_channel:call(Channel, 
			  #'queue.declare'{queue = QueueName,
					   durable = true}),
    amqp_channel:call(Channel, #'basic.qos'{prefetch_count = 1}),
    Queue.

assign_task(Channel, Msg) ->    
    amqp_channel:cast(Channel,
                      #'basic.publish'{
			 exchange = <<"">>,
			 routing_key = <<"wotan_task_queue">>},
                      #amqp_msg{
			 props = #'P_basic'{delivery_mode = 2},
			 payload = Msg}).