-module(rebar3_kjell).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_kjell_prv:init(State),
    {ok, State1}.
