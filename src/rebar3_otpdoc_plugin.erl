-module(rebar3_otpdoc_plugin).

-export([init/1]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = rebar3_otpdoc_plugin_prv:init(State),
    {ok, State1}.
