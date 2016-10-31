%%--------------------------------------------
%% @author 503407245@qq.com
%%--------------------------------------------

-module(gen_server_boot).
-behaviour(gen_server).

-export([start_link/4, start_link/3, init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-export([
  stop/1, stop/2,
  invoke_async/2, invoke_async/3, invoke_async/4,
  invoke_sync/2, invoke_sync/3, invoke_sync/4,
  invoke_send_after/3, invoke_send_after/4, invoke_send_after/5
]).

-record(boot_state,{mod,state}).

-callback init(Args :: term()) ->
  {ok, State :: term()} | {ok, State :: term(), timeout() | hibernate} |
  {stop, Reason :: term()} | ignore.
-callback handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: term()) ->
  {reply, Reply :: term(), NewState :: term()} |
  {reply, Reply :: term(), NewState :: term(), timeout() | hibernate} |
  {noreply, NewState :: term()} |
  {noreply, NewState :: term(), timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
  {stop, Reason :: term(), NewState :: term()}.
-callback handle_cast(Request :: term(), State :: term()) ->
  {noreply, NewState :: term()} |
  {noreply, NewState :: term(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: term()}.
-callback handle_info(Info :: timeout | term(), State :: term()) ->
  {noreply, NewState :: term()} |
  {noreply, NewState :: term(), timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: term()}.
-callback terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: term()) ->
  term().
-callback code_change(OldVsn :: (term() | {down, term()}), State :: term(),
    Extra :: term()) ->
  {ok, NewState :: term()} | {error, Reason :: term()}.

-define(INVOKE_FUNCTION(F), {invoke_function, F}).
-define(INVOKE_FUNCTION(F, A), {invoke_function, F, A}).
-define(INVOKE_FUNCTION(M, F, A), {invoke_function, M, F, A}).

%%---------------------------------------------------------
%% API
%%---------------------------------------------------------

stop(Reason) ->
  self() ! {stop, Reason}.

stop(Pid, Reason) ->
  Pid ! {stop, Reason}.

%% 匿名函数
invoke_async(Pid, F) when is_function(F, 1) ->
  gen_server:cast(Pid, ?INVOKE_FUNCTION(F)).
invoke_async(Pid, F, A) ->
  gen_server:cast(Pid, ?INVOKE_FUNCTION(F, A)).
invoke_async(Pid, M, F, A) ->
  gen_server:cast(Pid, ?INVOKE_FUNCTION(M, F, A)).

invoke_sync(Pid, F) when is_function(F, 1) ->
  gen_server:call(Pid, ?INVOKE_FUNCTION(F)).
invoke_sync(Pid, F, A) ->
  gen_server:call(Pid, ?INVOKE_FUNCTION(F, A)).
invoke_sync(Pid, M, F, A) ->
  gen_server:call(Pid, ?INVOKE_FUNCTION(M, F, A)).

invoke_send_after(Time, Desc, F) ->
  erlang:send_after(Time, Desc, ?INVOKE_FUNCTION(F)).
invoke_send_after(Time, Desc, F, A) ->
  erlang:send_after(Time, Desc, ?INVOKE_FUNCTION(F, A)).
invoke_send_after(Time, Desc, M, F, A) ->
  erlang:send_after(Time, Desc, ?INVOKE_FUNCTION(M, F, A)).

%%---------------------------------------------------------
%% gen_server
%%---------------------------------------------------------
start_link(ServerName, Module, Args, Options) ->
  gen_server:start_link(ServerName, ?MODULE, [Module,Args], Options).

start_link(Module, Args, Options) ->
  gen_server:start_link(?MODULE, [Module,Args], Options).

init([Module,Args]) ->
  case catch Module:init(Args) of
    {ok, State} ->
      {ok,#boot_state{mod = Module,state = State}};
    ErrorReason ->
      {stop, ErrorReason}
  end.


%%---------------------------------------------------------
%% @doc handle_call
%%---------------------------------------------------------
handle_call(?INVOKE_FUNCTION(F), _From, BootState = #boot_state{mod = Module, state = State}) ->
  try
    case  F(State) of
      ok ->
        {reply, ok, BootState};
      {ok, Reply} ->
        {reply, Reply, BootState};
      {ok, Reply, NewState} ->
        {reply, Reply, BootState#boot_state{state = NewState}}
    end
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w invoke_sync error:~w, stack:~w", [Module, {Error, ErrorReason}, erlang:get_stacktrace()]),
      {reply, {Error, ErrorReason}, BootState}
  end;

handle_call(?INVOKE_FUNCTION(F, A), _From, BootState = #boot_state{mod = Module}) ->
  try
    {reply, erlang:apply(F, A), BootState}
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w invoke_sync error:~w, stack:~w", [Module, {Error, ErrorReason}, erlang:get_stacktrace()]),
      {reply, {Error, ErrorReason}, BootState}
  end;

handle_call(?INVOKE_FUNCTION(M, F, A), _From, BootState = #boot_state{mod = Module}) ->
  try
    {reply, erlang:apply(M, F, A), BootState}
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w invoke_sync error:~w, stack:~w", [Module, {Error, ErrorReason}, erlang:get_stacktrace()]),
      {reply, {Error, ErrorReason}, BootState}
  end;

handle_call(Request, From, BootState = #boot_state{mod = Module,state = State}) ->
  try
    case Module:handle_call(Request,From,State) of
      {reply,Reply,NewState} ->
        {reply,Reply,BootState#boot_state{state = NewState}};
      {reply,Reply,NewState,hibernate} ->
        {reply,Reply,BootState#boot_state{state = NewState},hibernate};
      {reply,Reply,NewState,Timeout} ->
        {reply,Reply,BootState#boot_state{state = NewState},Timeout};
      {stop,Reason,Reply,NewState} ->
        {stop,Reason,Reply,BootState#boot_state{state = NewState}};
      {stop,Reason,NewState} ->
        {stop,Reason,BootState#boot_state{state = NewState}}
    end
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w handle_call error:~w, stack:~w",[Module, {Error, ErrorReason}, erlang:get_stacktrace()]),
      {reply, {Error, ErrorReason}, BootState}
  end.

%%---------------------------------------------------------
%% @doc handle_cast
%%---------------------------------------------------------
handle_cast(?INVOKE_FUNCTION(F), BootState = #boot_state{mod = Module, state = State}) ->
  try
    case F(State) of
      ok ->
        {noreply, BootState};
      {ok, NewState} ->
        {noreply, BootState#boot_state{state = NewState}}
    end
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w invoke_async error:~w, stack:~w", [Module, {Error, ErrorReason}, erlang:get_stacktrace()]),
      {noreply, BootState}
  end;

handle_cast(?INVOKE_FUNCTION(F, A), BootState = #boot_state{mod = Module}) ->
  try
    erlang:apply(F, A)
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w invoke_async error:~w, stack:~w", [Module, {Error, ErrorReason}, erlang:get_stacktrace()])
  end,
  {noreply, BootState};

handle_cast(?INVOKE_FUNCTION(M, F, A), BootState = #boot_state{mod = Module}) ->
  try
    erlang:apply(M, F, A)
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w invoke_async error:~w, stack:~w", [Module, {Error, ErrorReason}, erlang:get_stacktrace()])
  end,
  {noreply, BootState};

handle_cast(Request, BootState = #boot_state{mod = Module,state = State}) ->
  try
    case Module:handle_cast(Request,State) of
      {noreply,NewState} ->
        {noreply,BootState#boot_state{state = NewState}};
      {noreply,NewState,hibernate} ->
        {noreply,BootState#boot_state{state = NewState},hibernate};
      {noreply,NewState,Timeout} ->
        {noreply,BootState#boot_state{state = NewState},Timeout};
      {stop,Reason,NewState} ->
        {stop,Reason,BootState#boot_state{state = NewState}}
    end
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w handle_cast error:~w, stack:~w",[Module, {Error, ErrorReason}, erlang:get_stacktrace()]),
      {noreply, BootState}
  end.

%%---------------------------------------------------------
%% @doc handle_info
%%---------------------------------------------------------
handle_info({stop, Reason}, BootState) ->
  error_logger:info_msg("Module:~w handle_stop Reason:~w",[BootState#boot_state.mod, Reason]),
  {stop, Reason, BootState};

handle_info(?INVOKE_FUNCTION(F), BootState = #boot_state{mod = Module, state = State}) ->
  try
    case F(State) of
      ok ->
        {noreply, BootState};
      {ok, NewState} ->
        {noreply, BootState#boot_state{state = NewState}}
    end
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w invoke_send_after error:~w, stack:~w", [Module, {Error, ErrorReason}, erlang:get_stacktrace()]),
      {noreply, BootState}
  end;

handle_info(?INVOKE_FUNCTION(F, A), BootState = #boot_state{mod = Module}) ->
  try
    erlang:apply(F, A)
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w invoke_send_after error:~w, stack:~w", [Module, {Error, ErrorReason}, erlang:get_stacktrace()])
  end,
  {noreply, BootState};

handle_info(?INVOKE_FUNCTION(M, F, A), BootState = #boot_state{mod = Module}) ->
  try
    erlang:apply(M, F, A)
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w invoke_send_after error:~w, stack:~w", [Module, {Error, ErrorReason}, erlang:get_stacktrace()])
  end,
  {noreply, BootState};

handle_info(Request, BootState = #boot_state{mod = Module,state = State}) ->
  try
    case Module:handle_info(Request,State) of
      {noreply,NewState} ->
        {noreply,BootState#boot_state{state = NewState}};
      {noreply,NewState,hibernate} ->
        {noreply,BootState#boot_state{state = NewState},hibernate};
      {noreply,NewState,Timeout} ->
        {noreply,BootState#boot_state{state = NewState},Timeout};
      {stop,Reason,NewState} ->
        {stop,Reason,BootState#boot_state{state = NewState}}
    end
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w handle_info error:~w, stack:~w",[Module, {Error, ErrorReason}, erlang:get_stacktrace()]),
      {noreply, BootState}
  end.

%%---------------------------------------------------------
%% @doc terminate
%%---------------------------------------------------------
terminate(Reason, #boot_state{mod = Module,state = State}) ->
  try Module:terminate(Reason,State)
  catch Error: ErrorReason ->
    error_logger:error_msg("Module:~w terminate error:~w, stack:~w",[Module, {Error, ErrorReason}, erlang:get_stacktrace()])
  end,
  ok.

%%---------------------------------------------------------
%% @doc code_change
%%---------------------------------------------------------
code_change(OldVsn, BootState = #boot_state{mod = Module,state = State}, Extra) ->
  try
    case Module:code_change(OldVsn,State,Extra) of
      {ok,NewState} ->
        {ok,BootState#boot_state{state = NewState}};
      {error,Reason} ->
        {error,Reason}
    end
  catch
    Error : ErrorReason ->
      error_logger:error_msg("Module:~w code_change error:~w, stack:~w",[Module, {Error, ErrorReason}, erlang:get_stacktrace()]),
      {error, {Error, ErrorReason}}
  end.




