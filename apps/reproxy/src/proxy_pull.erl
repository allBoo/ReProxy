%%%-------------------------------------------------------------------
%%% @author alboo
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Авг. 2016 14:00
%%%-------------------------------------------------------------------
-module(proxy_pull).
-author("alboo").

-behaviour(gen_server).
-include_lib("reproxy.hrl").

%% API
-export([
  start_link/0,
  lock/1,
  release/1
]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  running = #{}  :: #{},
  port_uses = #{} :: #{},
  free_ports = [] :: [integer()],
  last_port  = 0  :: integer(),
  concurrent = 1  :: integer()
}).

-define(DEF_START_PORT, 10000).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


lock(Client) ->
  gen_server:call(?MODULE, {lock, Client}).

release(Pid) ->
  gen_server:cast(?MODULE, {release, Pid}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([]) ->
  RunningPorts = proxy_sup:find_exists(),
  {FreePorts, LastPort} = calc_free_ports(RunningPorts),
  Concurrent = config:get('proxy.concurrent', 1, integer),

  {ok, #state{free_ports = FreePorts, last_port = LastPort, concurrent = Concurrent}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_call({lock, Client}, _From, State) when length(State#state.free_ports) == 0 ->
  Port = State#state.last_port + 2,
  Proxy = proxy_sup:start_proxy(Client, Port),
  erlang:monitor(process, Proxy),

  PortUsesCnt = maps:get(Port, State#state.port_uses, 0) + 1,
  PortUses = (State#state.port_uses)#{Port => PortUsesCnt},
  Running = (State#state.running)#{Proxy => Port},

  FreePorts = if
                PortUsesCnt < State#state.concurrent ->
                  [Port];

                true -> []
              end,

  %?DBG("PortUses ~p, FreePorts ~p, Running ~p", [PortUses, FreePorts, Running]),
  {reply, {ok, Proxy}, State#state{port_uses = PortUses, last_port = Port, free_ports = FreePorts, running = Running}};


handle_call({lock, Client}, _From, #state{free_ports = [Port | Free]} = State) ->
  Proxy = proxy_sup:start_proxy(Client, Port),
  erlang:monitor(process, Proxy),

  PortUsesCnt = maps:get(Port, State#state.port_uses, 0) + 1,
  PortUses = (State#state.port_uses)#{Port => PortUsesCnt},
  Running = (State#state.running)#{Proxy => Port},

  FreePorts = if
                PortUsesCnt < State#state.concurrent ->
                  State#state.free_ports;

                true -> Free
              end,

  %?DBG("PortUses ~p, FreePorts ~p, Running ~p", [PortUses, FreePorts, Running]),
  {reply, {ok, Proxy}, State#state{port_uses = PortUses, free_ports = FreePorts, running = Running}};



handle_call(_Request, _From, State) ->
  {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_cast({release, Pid}, State) ->
  case maps:get(Pid, State#state.running, 0) of
    Port when Port > 0 ->
      ?LOG("Release port ~p", [Port]),
      Running = maps:remove(Pid, State#state.running),

      PortUsesCnt = maps:get(Port, State#state.port_uses, 0) - 1,
      PortUses = if
                   PortUsesCnt < 1 ->
                     try
                       maps:remove(Port, State#state.port_uses)
                     catch
                         _:_ ->
                           State#state.port_uses
                     end;

                   true ->
                     (State#state.port_uses)#{Port => PortUsesCnt}
                 end,

      FreePorts = case lists:member(Port, State#state.free_ports) of
                    true -> State#state.free_ports;
                    _    -> State#state.free_ports ++ [Port]
                  end,

      %?DBG("PortUses ~p, FreePorts ~p, Running ~p", [PortUses, FreePorts, Running]),
      {noreply, State#state{free_ports = FreePorts, port_uses = PortUses, running = Running}};

    _ ->
      {noreply, State}
  end;

handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).

handle_info({'DOWN', _, _, Pid, _}, State) ->
  ?DBG("Proxy down ~p", [Pid]),
  release(Pid),
  {noreply, State};

handle_info(_Info, State) ->
  ?DBG("INFO ~p", [_Info]),
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

calc_free_ports([]) -> {[], config:get('proxy.from_port', ?DEF_START_PORT, integer)};
calc_free_ports([BusyPort | []]) -> calc_free_ports([?DEF_START_PORT, BusyPort]);
calc_free_ports(BusyPorts) ->
  FreePorts = calc_free_ports(lists:sort(BusyPorts), []),
  LastPort = if
               length(FreePorts) > 0 -> lists:max(FreePorts);
               true -> ?DEF_START_PORT
             end,
  {FreePorts, LastPort}.

calc_free_ports([], Acc) -> Acc;
calc_free_ports([Port | []], Acc) -> Acc ++ [Port];
calc_free_ports([Port1, Port2 | Tail], Acc) ->
  Free = lists:seq(Port1, Port2, 2) -- [Port2],
  calc_free_ports([Port2] ++ Tail, Acc ++ Free).
