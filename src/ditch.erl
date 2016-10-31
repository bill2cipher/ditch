%%%-------------------------------------------------------------------
%%% @author jacky
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. 七月 2016 14:29
%%%-------------------------------------------------------------------
-module(ditch).
-author("jacky").

%%%% API
%%-export([start/0, stop/0]).
%%
%%
%%%% Start the whole world
%%start() ->
%%  application:ensure_all_started(ditch).
%%
%%
%%%% Stop the whole world
%%stop() ->
%%  application:stop(ditch),
%%  erlang:halt().

-export([start_listener/5]).
-export([stop_listener/1]).

stop_listener(Ref) ->
  case supervisor:terminate_child(ditch_sup, {ditch_listener_sup, Ref}) of
    ok ->
      supervisor:delete_child(ditch_sup, {ditch_listener_sup, Ref});
    Other ->
      Other
  end.

start_listener(Ref, NbListener, DitchOpts, Protocol, ProtoOpts)
    when is_atom(Ref) andalso is_integer(NbListener) andalso is_atom(Protocol) ->
  code:ensure_loaded(Protocol),
  ListenSpec = listen_spec(Ref, DitchOpts, Protocol, ProtoOpts),
  ServerSpec = server_spec(Ref, NbListener),
  {ok, SupPID} = supervisor:start_child(ditch_sup, ListenSpec),
  {ok, ServerPID} = supervisor:start_child(ditch_sup, ServerSpec),
  ditch_server:set_listener_sup(ServerPID, SupPID).

listen_spec(Ref, DitchOpts, Protocol, ProtoOpts) ->
  {{ditch_listener_sup, Ref},
   {ditch_listener_sup, start_link, [DitchOpts, Protocol, ProtoOpts]},
    permanent, infinity, supervisor, [ditch_listener_sup]}.

server_spec(Ref, NbListener) ->
  {{ditch_server, Ref},
    {ditch_server, start_link, [Ref, NbListener]},
    permanent, infinity, worker, [ditch_server]}.
