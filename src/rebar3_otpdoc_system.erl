%%
%% Copyright (C) 2016 Björn-Egil Dahlberg
%%
%% File:    rebar3_otpdoc_system.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2016-10-06
%%

-module(rebar3_otpdoc_system).
-export([run/2,
         run/3]).

-define(DBG(Format,Args), rebar_api:debug(Format,Args)).
-define(ABORT(Format,Args), rebar_api:abort(Format,Args)).

%% run programs and collect output.
run(Program0, Args) -> run(".", Program0, Args).
run(Cwd, Program0, Args) when is_list(Cwd) ->
    Program = case os:find_executable(Program0) of
                  Path when is_list(Path) ->
                      Path;
                  false ->
                      ?ABORT("Unable to find program: '~ts'\n", [Program0])
              end,
    Options = [{args,Args},binary,exit_status,stderr_to_stdout,
               {line,4096}, {cd, Cwd}],
    ?DBG("exec: ~ts", [[Program] ++ [" " ++ Arg || Arg <- Args]]),
    try open_port({spawn_executable,Program}, Options) of
        Port ->
            run_loop(Port, [])
    catch
        error:_ ->
            ?ABORT("Failed to execute: '~ts'\n", [Program])
    end.

run_loop(Port, Output) ->
    receive
        {Port,{exit_status,Status}} ->
            {Status,lists:reverse(Output)};
        {Port,{data,{eol,Bin}}} ->
            run_loop(Port, [Bin|Output]);
        _Msg ->
            run_loop(Port, Output)
    end.
