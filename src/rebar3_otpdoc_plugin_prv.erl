-module(rebar3_otpdoc_plugin_prv).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, otpdoc).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 otpdoc"},   % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "A rebar3 plugin for building OTP documentation"},
            {desc, "A rebar3 plugin for building OTP documentation"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [make_otpdoc(AppInfo) || AppInfo <- rebar_state:project_apps(State)],
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%%-----------------------------------------------------------------
make_otpdoc(AppInfo) ->
    io:format("~p~n", [rebar_app_info:name(AppInfo)]),
    Opts = rebar_app_info:opts(AppInfo),
    OtpOpts = proplists:unfold(rebar_opts:get(Opts, otpdoc_opts, [])),
    %/ldisk/egil/git/otp/lib/erl_docgen/priv/bin/xml_from_edoc.escript
    OutDir = rebar_app_info:out_dir(AppInfo),
    DocSrc = filename:join([rebar_app_info:dir(AppInfo),"doc","src"]),
    io:format("Opts: ~p~n", [OtpOpts]),
    Modules = proplists:get_value(edoc_modules, OtpOpts),
    _ = [edoc_module_to_xml(filename:join(["src",Mod])++".erl", []) || Mod <- Modules],
    XmlFiles = filelib:wildcard(filename:join(DocSrc,"*.xml")),
    Details = rebar_app_info:app_details(AppInfo),
    io:format("Details: ~p~n", [Details]),
    io:format("XmlFiles: ~p~n", [XmlFiles]),
    {Details,XmlFiles}.

edoc_module_to_xml(File,_Opts0) ->
    case filelib:is_regular(File) of
	true ->
	    Opts = [{def,            []},
		    {includes,       []},
		    {preprocess,     false},
		    {sort_functions, true},
		    {app_default,    "OTPROOT"},
		    {file_suffix,    ".xml"},
		    {dir,            "doc/src"},
		    {layout,         docgen_edoc_xml_cb}],
	    edoc:file(File, Opts);
	false ->
	    io:format("~s: not a regular file\n", [File]),
            error
    end.

edoc_users_guide_to_xml(File,_Opts0) ->
    case filelib:is_regular(File) of
	true ->
            Enc = epp:read_encoding(File, [{in_comment_only, false}]),
            Encoding = [{encoding, Enc} || Enc =/= none],
	    Opts = [{def,         []},
		    {app_default, "OTPROOT"},
		    {file_suffix, ".xml"},
		    {layout,       docgen_edoc_xml_cb}|Encoding],
	    Env = edoc_lib:get_doc_env(Opts),
	    {ok, Tags} = edoc_extract:file(File, overview, Env, Opts),
	    Data = edoc_data:overview("Overview", Tags, Env, Opts),
	    F = fun(M) -> M:overview(Data, Opts) end,
	    Text = edoc_lib:run_layout(F, Opts),
	    OutFile = "chapter.xml",
	    edoc_lib:write_file(Text, "doc/src", OutFile, Encoding);
	false ->
	    io:format("~s: not a regular file\n", [File]),
	    error
    end.

%% Ex. "October  6, 2016"
datestring() -> datestring(erlang:date()).
datestring({Y,M,D}) ->
    monthstring(M) ++ " " ++ integer_to_list(D) ++ ", " ++ integer_to_list(Y).

monthstring(M) ->
    case M of
        1 -> "January";
        2 -> "February";
        3 -> "March";
        4 -> "April";
        5 -> "May";
        6 -> "June";
        7 -> "July";
        8 -> "August";
        9 -> "September";
        10 -> "October";
        11 -> "November";
        12 -> "December"
    end.
