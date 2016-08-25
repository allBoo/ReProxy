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
  used = []  :: [{pid(), integer()}],
  used_ports = [] :: [integer()],
  free_ports = [] :: [integer()],
  last_port  = 0  :: integer()
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

  {ok, #state{free_ports = FreePorts, last_port = LastPort}}.

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
  UsedPorts = State#state.used_ports ++ [Port],
  Used = State#state.used ++ [{Proxy, Port}],
  erlang:monitor(process, Proxy),
  {reply, {ok, Proxy}, State#state{used_ports = UsedPorts, last_port = Port, used = Used}};

handle_call({lock, Client}, _From, #state{free_ports = [Port | FreePorts]} = State) ->
  Proxy = proxy_sup:start_proxy(Client, Port),
  UsedPorts = State#state.used_ports ++ [Port],
  Used = State#state.used ++ [{Proxy, Port}],
  erlang:monitor(process, Proxy),
  {reply, {ok, Proxy}, State#state{used_ports = UsedPorts, free_ports = FreePorts, used = Used}};



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
  case lists:keytake(Pid, 1, State#state.used) of
    {value, {Pid, Port}, Used} ->
      ?LOG("Release port ~p", [Port]),
      FreePorts = State#state.free_ports ++ [Port],
      UsedPorts = State#state.used_ports -- [Port],
      {noreply, State#state{free_ports = FreePorts, used_ports = UsedPorts, used = Used}};

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
