-module(ar_mine_randomx).

-on_load(init_nif/0).

-export([init_fast/2, hash_fast/2, init_light/1, hash_light/2]).
-export([release_state/1]).
-export([bulk_hash_fast/4]).

%% These exports are required for the DEBUG mode, where these functions are unused.
%% Also, these functions are used in ar_mine_randomx_tests.
-export([init_fast_nif/4, hash_fast_nif/5, bulk_hash_fast_nif/9]).
-export([init_light_nif/3, hash_light_nif/5]).

-include("ar.hrl").

-ifdef(DEBUG).
init_fast(_Key, _Threads) ->
	<<"state">>.
-else.
init_fast(Key, Threads) ->
	{ok, FastState} = init_fast_nif(Key, jit(), large_pages(), Threads),
	FastState.
-endif.

-ifdef(DEBUG).
hash_fast(_FastState, Data) ->
	Hash = crypto:hash(sha256, Data),
	list_to_binary(lists:sublist(binary_to_list(Hash), 48)).
-else.
hash_fast(FastState, Data) ->
	{ok, Hash} =
		hash_fast_nif(FastState, Data, jit(), large_pages(), hardware_aes()),
	Hash.
-endif.

-ifdef(DEBUG).
bulk_hash_fast(_FastState, {Nonce, _}, BDS, _Diff) ->
	Hash = crypto:hash(sha256, <<Nonce/binary, BDS/binary>>),
	{list_to_binary(lists:sublist(binary_to_list(Hash), 48)), Nonce, Nonce, 1}.
-else.
bulk_hash_fast(FastState, {Nonce1, Nonce2}, BDS, Diff) ->
	{ok, Hash, HashNonce, ExtraNonce, HashesTried} =
		bulk_hash_fast_nif(FastState, Nonce1, Nonce2, BDS, binary:encode_unsigned(Diff, big), jit(), large_pages(), hardware_aes(), ar_meta_db:get(randomx_bulk_hashing_iterations)),
	{Hash, HashNonce, ExtraNonce, HashesTried}.
-endif.

init_light(Key) ->
	{ok, LightState} = init_light_nif(Key, jit(), large_pages()),
	LightState.

hash_light(LightState, Data) ->
	{ok, Hash} =
		hash_light_nif(LightState, Data, jit(), large_pages(), hardware_aes()),
	Hash.

release_state(State) ->
	case release_state_nif(State) of
		ok ->
			ar:info([ar_mine_randomx, succeeded_releasing_randomx_state]),
			ok;
		{error, Reason} ->
			ar:warn([ar_mine_randomx, failed_to_release_randomx_state, {reason, Reason}]),
			error
	end.

%% Internal

jit() ->
	case ar_meta_db:get(randomx_jit) of
		false ->
			0;
		_ ->
			1
	end.

large_pages() ->
	case ar_meta_db:get(randomx_large_pages) of
		true ->
			1;
		_ ->
			0
	end.

hardware_aes() ->
	case ar_meta_db:get(randomx_hardware_aes) of
		false ->
			0;
		_ ->
			1
	end.

init_fast_nif(_Key, _JIT, _LargePages, _Threads) ->
	erlang:nif_error(nif_not_loaded).

init_light_nif(_Key, _JIT, _LargePages) ->
	erlang:nif_error(nif_not_loaded).

hash_fast_nif(_State, _Data, _JIT, _LargePages, _HardwareAES) ->
	erlang:nif_error(nif_not_loaded).

bulk_hash_fast_nif(
	_State, _Nonce1, _Nonce2, _BDS, _Diff, _JIT, _LargePages, _HardwareAES, _HashingIterations
) ->
	erlang:nif_error(nif_not_loaded).

hash_light_nif(_State, _Data, _JIT, _LargePages, _HardwareAES) ->
	erlang:nif_error(nif_not_loaded).

release_state_nif(_State) ->
	erlang:nif_error(nif_not_loaded).

init_nif() ->
	PrivDir = code:priv_dir(arweave),
	ok = erlang:load_nif(filename:join([PrivDir, "arweave"]), 0).
