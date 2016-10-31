%%%-------------------------------------------------------------------
%%% @author jacky
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 七月 2016 16:57
%%%-------------------------------------------------------------------
-module(ditch_queue).
-author("jacky").

%% API
-export([new/0,
  len/1,
  drop/1,
  get/1,
  in/2,
  is_empty/1]).

-record(ditch_queue, {queue = queue:new(), len = 0}).

new() ->
  #ditch_queue{}.

len(Queue) ->
  Queue#ditch_queue.len.

drop(Queue) ->
  #ditch_queue{queue = Q, len = Len} = Queue,
  Q2 = queue:drop(Q),
  Queue#ditch_queue{queue = Q2, len = Len - 1}.

is_empty(#ditch_queue{len = Len}) when Len =:= 0 -> true;
is_empty(_) -> false.

get(#ditch_queue{queue = Q}) ->
  queue:get(Q).

in(Val, Queue) ->
  #ditch_queue{queue = Q, len = Len} = Queue,
  Q2 = queue:in(Val, Q),
  Queue#ditch_queue{queue = Q2, len = Len + 1}.
