%%%
%%% @doc Server to maintain a node state.
%%%
%%% NB: The potential_txs field stores the set of txs that contradict those
%%% already stored within the txs field. That is those that have the same last
%%% tx reference as one already held in txs.
%%%
%%% These txs are not dropped as when a block is a mined blockshadows do not
%%% redistribute the set mined on, only their IDs. As such they may be required
%%% to validate newly distributed blocks from other nodes in the network.
%%%

-module(ar_node_state).

-export([start/0, stop/1, all/1, lookup/2, update/2]).

%%%
%%% Public API.
%%%

%% @doc Start a node state server.
start() ->
	Pid = spawn(fun() ->
		server(ets:new(ar_node_state, [set, private, {keypos, 1}]))
	end),
	% Set initial state values.
	update(Pid, [
		{hash_list, not_joined},    % current full hashlist
		{wallet_list, []},          % current up to date walletlist
		{floating_wallet_list, []}, % up to date walletlist with new txs applied
		{height, 0},                % current height of the blockweave
		{gossip, undefined},        % Gossip protcol state
		{txs, []},                  % set of new txs to be mined into the next block
		{miner, undefined},         % PID of the mining process
		{mining_delay, 0},          % delay on mining, used for netework simulation
		{automine, false},          % boolean dictating if a node should automine
		{reward_addr, unclaimed},   % reward address for mining a new block
		{trusted_peers, []},        % set of trusted peers used to join on
		{waiting_txs, []},          % set of txs on timeout whilst network distribution occurs
		{potential_txs, []},        % set of valid but contradictory txs
		{tags, []},                 % nodes tags to apply to a block when mining
		{reward_pool, 0},           % current mining rewardpool of the weave
		{diff, 0},                  % current mining difficulty of the weave (no. of preceeding zero)
		{last_retarget, undefined}, % timestamp at which the last difficulty retarget occurred
		{weave_size, 0}             % current size of the weave in bytes (only inc. data tx size)
	]),
	{ok, Pid}.

%% @doc Stop a node worker.
stop(Pid) ->
	Pid ! stop,
	ok.

%% @doc Get all values from state, the return is a map of the keys
%% and values. The operation is atomic, all needed values must be
%% retrieved with one call. Between calls the state may change.
all(Pid) ->
	Pid ! {all, all, self()},
	receive
		{ok, Values} ->
			{ok, Values};
		{error, Error} ->
			{error, Error}
	after
		5000 ->
			{error, timeout}
	end.

%% @doc Get one or more values from state, the return is a map of the
%% keys and values. Non-existant keys will return 'undefined' as value.
%% The operation is atomic, all needed values must be retrieved with
%% one call. Between calls the state may change.
lookup(Pid, Keys) ->
	Pid ! {lookup, Keys, self()},
	receive
		{ok, Values} ->
			{ok, Values};
		{error, Error} ->
			{error, Error}
	after
		5000 ->
			{error, timeout}
	end.

%% @doc Set one or more values from state, input is a list of {Key, Value}
%% or a map. The operation is atomic, all needed values must be setted with
%% one call. Between calls the state may change.
update(Pid, KeyValues) ->
	Pid ! {update, KeyValues, self()},
	receive
		ok ->
			ok;
		{error, Error} ->
			{error, Error}
	after
		5000 ->
			{error, timeout}
	end.

%%%
%%% Server functions.
%%%

%% @doc Main server loop.
server(Tid) ->
	receive
		{Command, KeyValues, Sender} ->
			try handle(Tid, Command, KeyValues) of
				Result ->
					Sender ! Result,
					server(Tid)
			catch
				throw:Term ->
					ar:report( [ {'NodeStateEXCEPTION', {Term} } ]),
					server(Tid);
				exit:Term ->
					ar:report( [ {'NodeStateEXIT', Term} ] ),
					server(Tid);
				error:Term ->
					ar:report( [ {'NodeStateERROR', {Term, erlang:get_stacktrace()} } ]),
					server(Tid)
			end;
		stop ->
			ok
	end.

%% @doc Handle the individual server commands. Starving ets table has to be
%% avoided by any means. Only atoms are allowed as keys and changes have
%% to be done atomically.
handle(Tid, all, all) ->
	All = ets:match_object(Tid, '$1'),
	{ok, maps:from_list(All)};
handle(Tid, lookup, Keys) when is_list(Keys) ->
	case lists:all(fun is_atom/1, Keys) of
		true ->
			{ok, maps:from_list(lists:map(fun(Key) ->
				case ets:lookup(Tid, Key) of
					[{Key, Value}] -> {Key, Value};
					[]             -> {Key, undefined}
				end
			end, Keys))};
		_ ->
			{error, {invalid_node_state_keys, Keys}}
	end;
handle(Tid, lookup, Key) ->
	handle(Tid, lookup, [Key]);
handle(_Tid, update, []) ->
	ok;
handle(Tid, update, KeyValues) when is_list(KeyValues) ->
	case lists:all(fun({Key, _Value}) -> is_atom(Key) end, KeyValues) of
		true ->
			ets:insert_new(Tid, KeyValues),
			ok;
		_ ->
			{error, {invalid_node_state_keys, KeyValues}}
	end;
handle(Tid, update, {Key, Value}) ->
	handle(Tid, update, [{Key, Value}]);
handle(Tid, update, KeyValues) when is_map(KeyValues) ->
	handle(Tid, update, maps:to_list(KeyValues));
handle(_Tid, update, Any) ->
	{error, {invalid_node_state_values, Any}};
handle(_Tid, Command, _KeyValues) ->
	{error, {invalid_node_state_command, Command}}.

%%%
%%% EOF
%%%

