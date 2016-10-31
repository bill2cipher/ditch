%%%-------------------------------------------------------------------
%%% @author jacky
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. 六月 2016 16:16
%%%-------------------------------------------------------------------
-module(util).
-author("jacky").

%% API
-export([change_prefix/1,
  timenow/0,
  timenow_mill/0,
  get_db_node/0,
  get_n/3,
  binary_join/1,
  binary_split/2,
  filter_request/1]).

change_prefix(Prefix) ->
  F = io_lib:format("[~p] [%L] %I %l%n", [Prefix]),
  log4erl:change_format(app1, F).

timenow() ->
  {A, B, _} = os:timestamp(),
  A * 1000 * 1000 + B.

timenow_mill() ->
  {A, B, C} = os:timestamp(),
  (A * 1000000 + B) * 1000 + C / 1000.

get_db_node() ->
  'db_svr@127.0.0.1'.

get_n(Queue, 0, Done) ->  {lists:reverse(Done), Queue};
get_n(Queue, N, Done) ->
  case queue:out(Queue) of
    {{value, X}, NewQueue} -> get_n(NewQueue, N - 1, [X | Done]);
    {empty, NewQueue} -> {lists:reverse(Done), NewQueue}
  end.

-spec binary_join([binary()]) -> binary().
binary_join([]) ->
  <<>>;
binary_join([Part]) ->
  Part;
binary_join(List) ->
  lists:foldr(fun (A, B) ->
    if
      bit_size(B) > 0 -> <<A/binary, B/binary>>;
      true -> A
    end
  end, <<>>, List).

binary_split(Data, Len) ->
  binary_split2(Len, Data, []).
binary_split2(_, <<>>, Rslt) -> lists:reverse(Rslt);
binary_split2(Len, Data, Rslt) ->
  case Data of
    <<Head:Len/binary, Left>> -> binary_split2(Len, Left, [Head | Rslt]);
    _ -> binary_split2(Len, <<>>, [Data | Rslt])
  end.

filter_request(Req) ->
  filter_request2(Req, 1, erlang:tuple_size(Req)).

filter_request2(_Req, Pos, Limit) when Pos > Limit -> undefined;
filter_request2(Req, Pos, Limit) ->
  case erlang:element(Pos, Req) of
    undefined -> filter_request2(Req, Pos + 1, Limit);
    Field -> Field
  end.
