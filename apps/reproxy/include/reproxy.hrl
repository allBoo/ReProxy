%%%-------------------------------------------------------------------
%%% @author alboo
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Май 2016 22:29
%%%-------------------------------------------------------------------
-author("alboo").

%%% ====================================================================
%%% Main include file
%%% ====================================================================
-include_lib("log.hrl").
-include_lib("error.hrl").


%% Helper macro for declaring children of supervisor
-define(CHILD(I), {I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(CHILD(I, A), {I, {I, start_link, A}, permanent, 5000, worker, [I]}).
-define(CHILD(N, I, A), {N, {I, start_link, A}, permanent, 5000, worker, [I]}).
-define(CHILD_SUP(I), {I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).
-define(CHILD_SUP_T(I), {I, {I, start_link, []}, transient, infinity, supervisor, [I]}).

%%% ====================================================================
%%% Env spec
%%% ====================================================================
-record(env, {
  key,
  value
}).

%%% ====================================================================
%%% Data types
%%% ====================================================================
-define(IPV4, 16#01).
-define(IPV6, 16#04).
-define(DOMAIN, 16#03).

-define(OPTIONS,
  [binary,
    {reuseaddr, true},
    {active, false},
    {nodelay, true}
  ]).

-define(OPTIONS(IP), [{ip, IP} | ?OPTIONS]).

-define(GETADDR, fun(IP) -> {ok, Addr} = inet:getaddr(IP, inet), Addr end).
