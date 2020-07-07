%%%-------------------------------------------------------------------
%%% @author Eric des Courtis
%%% @copyright (C) 2020, f2pool
%%% @doc
%%%
%%% @end
%%% Created : 03. Jul 2020 2:11 p.m.
%%%-------------------------------------------------------------------
-module(ar_external_mining_tracker_srv).
-author("Eric des Courtis").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(ar_external_mining_tracker_srv_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) ->
  {ok, State :: #ar_external_mining_tracker_srv_state{}} | {ok, State :: #ar_external_mining_tracker_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  pg2:create(new_block_notifications),
  ok = pg2:join(new_block_notifications, self()),
  ar:info("External mining tracker starting...~n", []),
  timer:send_after(100, check_block_index),
  {ok, #ar_external_mining_tracker_srv_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #ar_external_mining_tracker_srv_state{}) ->
  {reply, Reply :: term(), NewState :: #ar_external_mining_tracker_srv_state{}} |
  {reply, Reply :: term(), NewState :: #ar_external_mining_tracker_srv_state{}, timeout() | hibernate} |
  {noreply, NewState :: #ar_external_mining_tracker_srv_state{}} |
  {noreply, NewState :: #ar_external_mining_tracker_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #ar_external_mining_tracker_srv_state{}} |
  {stop, Reason :: term(), NewState :: #ar_external_mining_tracker_srv_state{}}).
handle_call(_Request, _From, State = #ar_external_mining_tracker_srv_state{}) ->
  {reply, ok, State}.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #ar_external_mining_tracker_srv_state{}) ->
  {noreply, NewState :: #ar_external_mining_tracker_srv_state{}} |
  {noreply, NewState :: #ar_external_mining_tracker_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #ar_external_mining_tracker_srv_state{}}).
handle_cast(_Request, State = #ar_external_mining_tracker_srv_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #ar_external_mining_tracker_srv_state{}) ->
  {noreply, NewState :: #ar_external_mining_tracker_srv_state{}} |
  {noreply, NewState :: #ar_external_mining_tracker_srv_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #ar_external_mining_tracker_srv_state{}}).
handle_info(check_block_index, State) ->
  case whereis(http_entrypoint_node) of
    undefined -> ok;
    Pid ->
      ar:info("Block index is: ~p~n", [ar_node:get_block_index(Pid)])
  end,
  timer:send_after(100, check_block_index),
  {noreply, State};
handle_info(Info, State = #ar_external_mining_tracker_srv_state{}) ->
  ar:info("External mining tracker received: ~p~n", [Info]),
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #ar_external_mining_tracker_srv_state{}) -> term()).
terminate(_Reason, _State = #ar_external_mining_tracker_srv_state{}) ->
  ok.

%% @private
%% @doc Convert process state when code is changed
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #ar_external_mining_tracker_srv_state{},
    Extra :: term()) ->
  {ok, NewState :: #ar_external_mining_tracker_srv_state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State = #ar_external_mining_tracker_srv_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
