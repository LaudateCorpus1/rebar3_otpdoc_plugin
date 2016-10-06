%%
%% Copyright (C) 2016 Björn-Egil Dahlberg
%%
%% File:    rebar3_otpdoc_system.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2016-10-06
%%

-module(rebar3_otpdoc_system).

-export([run/2,
         run/3,
         error_msg/2,
         format_error/2, format_error/3]).

%% run programs and collect output.
run(Program0, Args) -> run(".", Program0, Args).
run(Cwd, Program0, Args) when is_list(Cwd) ->
    Program = case os:find_executable(Program0) of
                  Path when is_list(Path) ->
                      Path;
                  false ->
                      error_msg("Unable to find program: ~s\n", [Program0])
              end,
    Options = [{args,Args},binary,exit_status,stderr_to_stdout,
               {line,4096}, {cd, Cwd}],
    %io:put_chars(standard_error, ["#executing: ", Program] ++ [" " ++ Arg || Arg <- Args] ++ "\n"),
    try open_port({spawn_executable,Program}, Options) of
        Port ->
            run_loop(Port, [])
    catch
        error:_ ->
            error_msg("Failed to execute ~s\n", [Program0])
    end.

run_loop(Port, Output) ->
    receive
        {Port,{exit_status,Status}} ->
            {Status,lists:reverse(Output)};
        {Port,{data,{eol,Bin}}} ->
            run_loop(Port, [Bin|Output]);
        _Msg ->
            %io:format(standard_error,"L: ~p~n", [_Msg]),
            run_loop(Port, Output)
    end.

error_msg(Format, Args) ->
    io:format(Format, Args),
    stop(1).

stop(Code) ->
    init:stop(Code),
    wait_for_stop().

wait_for_stop() ->
    receive
        _ -> wait_for_stop()
    end.

%% format_error
format_error(E,R) -> format_error(E,R,erlang:get_stacktrace()).
format_error(A,B,Cs) ->
    io:put_chars(standard_error, [
            io_lib:format("~w:~1000p", [A,B]), "\n",
            format_stack(Cs)
        ]).

format_stack([S|Stack]) ->
    [format_stack(S)|format_stack(Stack)];
format_stack({M,F,As}) ->
    [format_mfa(M,F,As),"\n"];
format_stack({M,F,As,Opts}) ->
    [format_mfa(M,F,As),
     format_fileline(
       proplists:get_value(file, Opts),
       proplists:get_value(line, Opts)), "\n"];
format_stack([]) -> [].

format_mfa(M,F,A) when is_integer(A) ->
    ["  in call from ",
        io_lib:format("~w:~w/~w", [M,F,A])];
format_mfa(M,F,As) when is_list(As) ->
    ["  in function
        ",io_lib:format("~w:~w(~s)",
        [M,F,format_arguments(As)])].

format_fileline(File, Line) when File =/= undefined, is_integer(Line) ->
    io_lib:format(" (~s:~w)", [File, Line]);
format_fileline(_, _) -> [].

format_arguments([I1,I2|Is]) ->
    [format_argument(I1), ", "|format_arguments([I2|Is])];
format_arguments([I]) ->
    [format_argument(I)];
format_arguments([]) -> [].

format_argument(I) -> io_lib:format("~w", [I]).
