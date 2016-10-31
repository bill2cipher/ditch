%%%-------------------------------------------------------------------
%%% @author jacky
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 七月 2016 19:02
%%%-------------------------------------------------------------------
-module(ditch_buffer_test).
-author("jacky").

-include("../include/ditch.hrl").
%% API
-export([test/0]).

test() ->
  Buf = ditch_buffer:new(256, undefined),
  Buf2 = test_insert(Buf),
  Buf3 = test_delete(Buf2),
  {Buf2, Buf3}.

test_insert(Buf) ->
  test_insert2(Buf, ?LAST_INDEX, 200).

test_insert2(Buf, ?LAST_INDEX, 0) -> Buf;
test_insert2(Buf, ?LAST_INDEX, Cnt) ->
  Buf2 = ditch_buffer:append(?LAST_INDEX, good, Buf),
  test_insert2(Buf2, ?LAST_INDEX, Cnt - 1).

test_delete(Buf) ->
  test_delete2(Buf, ditch_buffer:first(Buf), 200).

test_delete2(Buf, _, 0) -> Buf;
test_delete2(Buf, Idx, Cnt) ->
  Buf2 = ditch_buffer:delete(Idx, ?LAST_INDEX, Buf),
  test_delete2(Buf2, ditch_buffer:first(Buf2), Cnt - 1).
