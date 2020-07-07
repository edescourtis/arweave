%%%-------------------------------------------------------------------
%%% @author Eric des Courtis
%%% @copyright (C) 2020, f2pool
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2020 6:44 p.m.
%%%-------------------------------------------------------------------
-module(ar_external_mining).
-author("Eric des Courtis").

%% API
-export([start/0]).

start() ->
  Dispatch = cowboy_router:compile([
    {'_', [
      {"/", ar_external_mining_rpc, []}
    ]}
  ]),
  {ok, _} = cowboy:start_clear(http, [{port, 2387}], #{
    env => #{dispatch => Dispatch}
  }),
  ar_external_mining_sup:start_link().
