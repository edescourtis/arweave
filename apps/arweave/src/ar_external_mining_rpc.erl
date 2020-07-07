%%%-------------------------------------------------------------------
%%% @author Eric des Courtis
%%% @copyright (C) 2020, f2pool
%%% @doc
%%%
%%% @end
%%% Created : 02. Jul 2020 6:48 p.m.
%%%-------------------------------------------------------------------
-module(ar_external_mining_rpc).
-author("Eric des Courtis").

%% API
-export([
  init/2,
  allowed_methods/2,
  %%content_types_provided/2,
  content_types_accepted/2
]).

-export([
  json_rpc/2,
  handle_submitwork/3,
  handle_getwork/3
]).

init(Req, Opts) ->
  handle_getwork,
  handle_submitwork,
  {cowboy_rest, Req, Opts}.

allowed_methods(Req, State) ->
  {[<<"POST">>], Req, State}.

%%
%%content_types_provided(Req, State) ->
%%  {[
%%    {{<<"application">>, <<"json">>, []}, json_rpc}
%%  ], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, '*'}, json_rpc}
  ], Req, State}.

json_rpc(Req0, State0) ->
  {ok, Data, Req1} = cowboy_req:read_body(Req0),
  #{<<"jsonrpc">> := <<"2.0">>, <<"method">> := Method, <<"params">> := Params, <<"id">> := Id}
    = jiffy:decode(Data, [return_maps]),
  Handler = erlang:binary_to_existing_atom(<< "handle_", Method/binary >>, utf8),
  {Result, Req2, State1} = ?MODULE:Handler(Req1, Params, State0),
  Body = jiffy:encode(#{<<"jsonrpc">> => <<"2.0">>, <<"result">> => Result, <<"id">> => Id}),
  cowboy_req:reply(200, #{
    <<"content-type">> => <<"application/json">>
  }, Body, Req2),
  {stop, Req2, State1}.


handle_submitwork(Req, _Params, State) ->
  {true, Req, State}.

handle_getwork(Req, _Params, State) ->

  {true, Req, State}.