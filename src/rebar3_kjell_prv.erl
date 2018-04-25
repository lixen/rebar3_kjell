-module(rebar3_kjell_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, kjell).
-define(DEPS, [compile]).

-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).
-define(CRASHDUMP(Str, Args), rebar_log:crashdump(Str, Args)).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 kjell"}, % How to use the plugin
            {opts, []},
            {short_desc, "A rebar plugin"},
            {desc, "A rebar plugin"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(Config) ->
  shell(Config),
  {ok, Config}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
  io_lib:format("~p", [Reason]).

%% NOTE:
%% this is an attempt to replicate `erl -pa ./ebin -pa deps/*/ebin`. it is
%% mostly successful but does stop and then restart the user io system to get
%% around issues with rebar being an escript and starting in `noshell` mode.
%% it also lacks the ctrl-c interrupt handler that `erl` features. ctrl-c will
%% immediately kill the script. ctrl-g, however, works fine

shell(State) ->
  setup_name(State),
  setup_paths(State),
  setup_shell(),
  simulate_proc_lib(),
  true = register(rebar_agent, self()),
  {ok, GenState} = rebar_agent:init(State),
  %% Hack to fool the init process into thinking we have stopped and the normal
  %% node start process can go on. Without it, init:get_status() always return
  %% '{starting, started}' instead of '{started, started}'
  init ! {'EXIT', self(), normal},
  gen_server:enter_loop(rebar_agent, [], GenState, {local, rebar_agent}, hibernate).

setup_shell() ->
  OldUser = kill_old_user(),
  %% Test for support here
  NewUser = try erlang:open_port({spawn,"tty_sl -c -e"}, []) of
              Port when is_port(Port) ->
                true = port_close(Port),
                setup_new_shell()
            catch
              error:_ ->
                setup_old_shell()
            end,
  rewrite_leaders(OldUser, NewUser).

kill_old_user() ->
  OldUser = whereis(user),
  %% terminate the current user's port, in a way that makes it shut down,
  %% but without taking down the supervision tree so that the escript doesn't
  %% fully die
  [P] = [P || P <- element(2,process_info(whereis(user), links)), is_port(P)],
  user ! {'EXIT', P, normal}, % pretend the port died, then the port can die!
  exit(P, kill),
  wait_for_port_death(1000, P),
  OldUser.

wait_for_port_death(N, _) when N < 0 ->
  %% This risks displaying a warning!
  whatever;
wait_for_port_death(N, P) ->
  case erlang:port_info(P) of
    undefined ->
      ok;
    _ ->
      timer:sleep(10),
      wait_for_port_death(N-10, P)
  end.

setup_new_shell() ->
  %% terminate the current user supervision structure, if any
  _ = supervisor:terminate_child(kernel_sup, user),
  %% start a new shell (this also starts a new user under the correct group)
  start_kjell:start(),

  %% wait until user_drv and user have been registered (max 3 seconds)
  ok = wait_until_user_started(3000),
  whereis(user).

setup_old_shell() ->
  %% scan all processes for any with references to the old user and save them to
  %% update later
  NewUser = rebar_user:start(), % hikack IO stuff with fake user
  NewUser = whereis(user),
  NewUser.

rewrite_leaders(OldUser, NewUser) ->
  %% set any process that had a reference to the old user's group leader to the
  %% new user process. Catch the race condition when the Pid exited after the
  %% liveness check.
  _ = [catch erlang:group_leader(NewUser, Pid)
    || Pid <- erlang:processes(),
    proplists:get_value(group_leader, erlang:process_info(Pid)) == OldUser,
    is_process_alive(Pid)],
  %% Application masters have the same problem, but they hold the old group
  %% leader in their state and hold on to it. Re-point the processes whose
  %% leaders are application masters. This can mess up a few things around
  %% shutdown time, but is nicer than the current lock-up.
  OldMasters = [Pid
    || Pid <- erlang:processes(),
    Pid < NewUser, % only change old masters
    {_,Dict} <- [erlang:process_info(Pid, dictionary)],
    {application_master,init,4} == proplists:get_value('$initial_call', Dict)],
  _ = [catch erlang:group_leader(NewUser, Pid)
    || Pid <- erlang:processes(),
    lists:member(proplists:get_value(group_leader, erlang:process_info(Pid)),
      OldMasters)],
  try
    %% enable error_logger's tty output
    error_logger:swap_handler(tty),
    %% disable the simple error_logger (which may have been added multiple
    %% times). removes at most the error_logger added by init and the
    %% error_logger added by the tty handler
    remove_error_handler(3),
    %% reset the tty handler once more for remote shells
    error_logger:swap_handler(tty)
  catch
    E:R -> % may fail with custom loggers
      ?DEBUG("Logger changes failed for ~p:~p (~p)", [E,R,erlang:get_stacktrace()]),
      hope_for_best
  end.


setup_paths(State) ->
  %% Add deps to path
  code:add_pathsa(rebar_state:code_paths(State, all_deps)),
  %% add project app test paths
  ok = add_test_paths(State).


simulate_proc_lib() ->
  FakeParent = spawn_link(fun() -> timer:sleep(infinity) end),
  put('$ancestors', [FakeParent]),
  put('$initial_call', {rebar_agent, init, 1}).

setup_name(State) ->
  {Long, Short, Opts} = rebar_dist_utils:find_options(State),
  rebar_dist_utils:either(Long, Short, Opts).


remove_error_handler(0) ->
  ?WARN("Unable to remove simple error_logger handler", []);
remove_error_handler(N) ->
  case gen_event:delete_handler(error_logger, error_logger, []) of
    {error, module_not_found} -> ok;
    {error_logger, _} -> remove_error_handler(N-1)
  end.

%% Timeout is a period to wait before giving up
wait_until_user_started(0) ->
  ?ABORT("Timeout exceeded waiting for `user` to register itself", []),
  erlang:error(timeout);
wait_until_user_started(Timeout) ->
  case whereis(user) of
    %% if user is not yet registered wait a tenth of a second and try again
    undefined -> timer:sleep(100), wait_until_user_started(Timeout - 100);
    _ -> ok
  end.

add_test_paths(State) ->
  _ = [begin
         AppDir = rebar_app_info:out_dir(App),
         %% ignore errors resulting from non-existent directories
         _ = code:add_path(filename:join([AppDir, "test"]))
       end || App <- rebar_state:project_apps(State)],
  _ = code:add_path(filename:join([rebar_dir:base_dir(State), "test"])),
  ok.
