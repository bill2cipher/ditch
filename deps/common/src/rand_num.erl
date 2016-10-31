%%%-------------------------------------------------------------------
%%% @author jacky
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 七月 2016 20:02
%%%-------------------------------------------------------------------
-module(rand_num).
-author("jacky").

-behaviour(gen_server).

%% API
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).
-export([start_link/0, next/0, next/1, next/2]).

-export([test/1]).

-record(random_state, {server}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, #random_state{}, []).
% next() -> gen_server:call(?MODULE, {get_random}).
% next(M) -> gen_server:call(?MODULE, {get_random, M}).
% next(M, N) -> gen_server:call(?MODULE, {get_random, M, N}).

%% 性能优化版本
next() ->
  [{seed, S}] = ets:lookup(random_seed_ets, seed),
  {R, S2} = random:uniform_s(S),
  ets:insert(random_seed_ets, {seed, S2}),
  R.
next(M) ->
  [{seed, S}] = ets:lookup(random_seed_ets, seed),
  {R, S2} = random:uniform_s(M, S),
  ets:insert(random_seed_ets, {seed, S2}),
  R.
next(M, N) ->
  [{seed, S}] = ets:lookup(random_seed_ets, seed),
  {R, S2} = random:uniform_s(S),
  ets:insert(random_seed_ets, {seed, S2}),
  M + round(R * (N - M)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(State) ->
  process_flag(trap_exit, true),
  {A,B,C} = now(),
  random:seed(A,B,C),
  ets:new(random_seed_ets, [set, public, named_table]),
  ets:insert(random_seed_ets, {seed, now()}),
  {ok, State}.

handle_call({get_random}, _From, State) ->
  Random = random:uniform(),
  {reply, Random, State};

handle_call({get_random, M}, _From, State) ->
  Random = random:uniform(M),
  {reply, Random, State};

handle_call({get_random, M, N}, _From, State) ->
  Random = random:uniform(),
  {reply, M + round(Random * (N - M)), State}.

terminate(_Reason, _State) -> log4erl:info("~p stopping", [?MODULE]), ok.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


test(N) ->
  statistics(wall_clock),
  [next() || _ <- lists:seq(1, N)],
  {_, Time} = statistics(wall_clock),
  io:format(">>> test1: ~p~n", [Time]).
