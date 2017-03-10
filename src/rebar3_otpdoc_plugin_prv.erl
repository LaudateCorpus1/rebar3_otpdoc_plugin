-module(rebar3_otpdoc_plugin_prv).
%%
%% useful rebar.config options
%% {otpdoc_opts, [{doc_src, string()},           %% default: $APP/doc/src
%%                {company, string()},           %% default: "Ericsson AB"
%%                {edoc_modules, [atom()]},      %% default: []
%%                {edoc_chapters, [string()]},   %% default: [], ex.: "doc/overview.edoc"
%%                {images, [string()]}           %% default: $APP/doc/src/**.{gif,jpeg,jpg,png}
%%               ]}.
%%
%%

-import(lists, [foreach/2]).

-export([init/1, do/1, format_error/1]).

-define(NAMESPACE, otp).
-define(PROVIDER, doc).
-define(DEPS, [{default,app_discovery}]).

-define(INFO(Format,Args), rebar_api:info(Format,Args)).
-define(DBG(Format,Args), rebar_api:debug(Format,Args)).
-define(WARN(Format,Args), rebar_api:warn(Format,Args)).
-define(ABORT(Format,Args), rebar_api:abort(Format,Args)).
-define(CONSOLE(Format,Args), rebar_api:console(Format,Args)).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {namespace, ?NAMESPACE},      % OTP
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 otp doc"},  % How to use the plugin
            {opts, [{target,$o,"target",string,help(target)},
                    {html_target,$t,"html_target",string,help(html_target)},
                    {man_target,$m,"man_target",string,help(man_target)}]},
            {short_desc, "A rebar3 plugin for building OTP documentation"},
            {desc, "A rebar3 plugin for building OTP documentation"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

help(target) ->
    "Directory where the documentation shall be installed. "
    "Both man pages and html documentation are installed, using 'man' and 'html' path prefix respectively.";
help(man_target) ->
    "Directory where the man pages shall be installed.";
help(html_target) ->
    "Directory where the html documentation shall be installed.".

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {CliOpts,_} = rebar_state:command_parsed_args(State),
    foreach(fun(AppInfo) ->
                    make_doc_otp(AppInfo, rebar_state:resources(State), CliOpts)
            end, rebar_state:project_apps(State)),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%%-----------------------------------------------------------------
make_doc_otp(AppInfo, Resources, CliOpts) ->
    Opts = rebar_app_info:opts(AppInfo),
    OtpOpts = proplists:unfold(rebar_opts:get(Opts, otpdoc_opts, [])),
    ?DBG("opts: ~p", [CliOpts]),
    ?DBG("otpdoc_opts: ~p", [OtpOpts]),
    OutDir = rebar_app_info:out_dir(AppInfo),
    DocSrc = filename:join([rebar_app_info:dir(AppInfo),"doc","src"]),
    Dir = rebar_app_info:dir(AppInfo),
    DefaultTarget = proplists:get_value(target, CliOpts, filename:join([OutDir,"doc"])),

    %% set context
    Ctx = #{name => rebar_app_info:name(AppInfo),
            vsn => rebar_utils:vcs_vsn(rebar_app_info:original_vsn(AppInfo),
                                       rebar_app_info:dir(AppInfo),
                                       Resources),
            app_dir => Dir,
            company => proplists:get_value(company, OtpOpts, "Ericsson AB"),
            doc_src => proplists:get_value(doc_src, OtpOpts, DocSrc),
            man_target => proplists:get_value(man_target, CliOpts, filename:join(DefaultTarget,"man")),
            html_target => proplists:get_value(html_target, CliOpts, filename:join(DefaultTarget,"html")),
            date => datestring(),
            erl_docgen => code:lib_dir(erl_docgen)},

    ?DBG("initial context: ~p", [Ctx]),

    %% convert edoc modules to xml-files
    EdocMods = proplists:get_value(edoc_modules, OtpOpts, []),
    ?DBG("edoc module files: ~p", [EdocMods]),
    foreach(fun (Mod) ->
                    edoc_module_to_xml(filename:join([Dir,"src",Mod])++".erl", Ctx)
            end, EdocMods),

    %% convert edoc chapters to xml-files
    EdocChapters = proplists:get_value(edoc_chapters, OtpOpts, []),
    ?DBG("edoc chapter files: ~p", [EdocChapters]),
    foreach(fun (Chapter) ->
                    edoc_users_guide_to_xml(Chapter,Ctx)
            end, EdocChapters),

    %% convert markdown .md-files to xml-files
    MdFiles = filelib:wildcard(filename:join(DocSrc, "*.{md,markdown}")),
    ?DBG(".md-files: ~p", [MdFiles]),
    foreach(fun (MdFile) ->
                    Filename = filename:rootname(filename:basename(MdFile)),
                    XmlFileOut = filename:join([DocSrc,Filename ++ ".xml"]),
                    rebar3_otpdoc_md_to_xml:translate(MdFile, XmlFileOut)
            end, MdFiles),

    %% convert .xmlsrc-files to include code examples
    XmlSrcFiles = filelib:wildcard(filename:join(DocSrc, "*.xmlsrc")),
    ?DBG(".xmlsrc-files for code-line processing: ~p", [XmlSrcFiles]),
    foreach(fun (XmlSrcFile) ->
                    Filename = filename:rootname(filename:basename(XmlSrcFile)),
                    XmlFileOut = filename:join([DocSrc,Filename ++ ".xml"]),
                    ok = xml_codeline_preprocessing(XmlSrcFile, XmlFileOut)
            end, XmlSrcFiles),

    %% generate specs
    ?INFO("Generating specification-files", []),
    ErlFiles = filelib:wildcard("src/*.erl", Dir),
    HrlFiles = lists:usort([filename:dirname(Hrl) || Hrl <- filelib:wildcard("{include,src}/*.hrl", Dir)]),
    ?DBG(".erl-files to generate specifications: ~p", [ErlFiles]),
    foreach(fun (ErlFile) ->
                    edoc_specs_to_xml(ErlFile,HrlFiles,Ctx)
            end, ErlFiles),

    %% find defined man-pages
    XmlFiles = filelib:wildcard(filename:join(DocSrc, "*.xml")),
    ?DBG(".xml-files to classify: ~p", [XmlFiles]),
    ManFiles = lists:foldl(fun(XmlFile, Acc) ->
                                   case classify_man_page(XmlFile) of
                                       none -> Acc;
                                       Type -> [{Type,XmlFile}|Acc]
                                   end
                           end, [], XmlFiles),
    ?DBG("classified .xml-files to generate man-pages: ~p", [ManFiles]),

    %% find doc-images
    Images = case proplists:get_value(images, OtpOpts) of
                 undefined ->
                     filelib:wildcard("**/*.{gif,jpeg,jpg,png}", DocSrc);
                 Imgs ->
                     Imgs
             end,
    ?DBG("image-files for html-pages: ~p", [Images]),

    %% set final context
    Ctx1 = Ctx#{specs => filename:join([DocSrc,"specs.xml"]),
                images => Images},

    ?DBG("context for documentation generation: ~p", [Ctx1]),
    ok = make_doc_html(Ctx1),
    ok = make_doc_man(ManFiles, Ctx1),
    ok.

edoc_module_to_xml(File, #{doc_src := DocSrc}) ->
    case filelib:is_regular(File) of
	true ->
            ?DBG("convert edoc module '~ts' to xml target '~ts'", [File,DocSrc]),
	    Opts = [{def,            []},
		    {includes,       []},
		    {preprocess,     false},
		    {sort_functions, true},
		    {app_default,    "OTPROOT"},
		    {file_suffix,    ".xml"},
		    {dir,            DocSrc},
		    {layout,         docgen_edoc_xml_cb}],
	    edoc:file(File, Opts);
	false ->
	    ?ABORT("~s: not a regular file", [File])
    end.

edoc_users_guide_to_xml(File, #{doc_src := DocSrc}) ->
    case filelib:is_regular(File) of
	true ->
            ?DBG("convert edoc chapter '~ts' to xml target '~ts'", [File,DocSrc]),
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
	    edoc_lib:write_file(Text, DocSrc, OutFile, Encoding);
	false ->
	    ?ABORT("~ts: not a regular file", [File])
    end.

edoc_specs_to_xml(File, InclFs, #{doc_src := DocSrc}) ->
    Dir = doc_src_to_specs(DocSrc),
    ReadOpts = [{includes, InclFs},
                {preprocess, true}],
    ExtractOpts = [{report_missing_type, false}],
    LayoutOpts = [{pretty_printer, erl_pp},
                  {layout, docgen_otp_specs}],
    try
        ?DBG("generate specifications for '~ts'", [File]),
        ?DBG("reading edoc source with opts: ~p", [ReadOpts]),
        Fs = clauses(edoc:read_source(File, ReadOpts)),
        Doc = extract(File, Fs, ExtractOpts),
        Text = edoc:layout(Doc, LayoutOpts),
        ok = write_text(Text, File, Dir),
        rename(Dir, File)
    catch
        _:_ ->
            clean_up(Dir),
            ?ABORT("edoc could not process file '~ts'", [File])
    end.

doc_src_to_specs(DocSrc) ->
    [_|Paths] = lists:reverse(filename:split(DocSrc)),
    filename:join(lists:reverse(["specs"|Paths])).

extract(File, Forms, Opts) ->
    Env = edoc_lib:get_doc_env([], [], []),
    {_Module, Doc} = edoc_extract:source(Forms, File, Env, Opts),
    Doc.

clauses(Fs) ->
    clauses(Fs, no).

clauses([], no) ->
    [];
clauses([F | Fs], Spec) ->
    case F of
        {attribute,_,spec,_} ->
            clauses(Fs, F);
        {function,_,_N,_A,_Cls} when Spec =/= no->
            {attribute,_,spec,{Name,FunTypes}} = Spec,
            %% [throw({no,Name,{_N,_A}}) || Name =/= {_N,_A}],
            %% EDoc doesn't care if a function appears more than once;
            %% this is how overloaded specs are handled:
            (lists:append([[setelement(4, Spec, {Name,[T]}),F] ||
                              T <- FunTypes])
             ++ clauses(Fs, no));
        _ ->
            [F | clauses(Fs, Spec)]
    end.

write_text(Text, File, Dir) ->
    Base = filename:basename(File, ".erl"),
    OutFile = filename:join(Dir, Base) ++ ".specs",
    ok = filelib:ensure_dir(OutFile),
    ?DBG("writing specifications to '~ts'", [OutFile]),
    case file:write_file(OutFile, Text) of
        ok ->
            ok;
        {error, R} ->
            R1 = file:format_error(R),
            ?ABORT("could not write file '~ts': ~s", [File, R1])
    end.

rename(Dir, F) ->
    Mod = filename:basename(F, ".erl"),
    Old = filename:join(Dir, Mod ++ ".specs"),
    New = filename:join(Dir, "specs_" ++ Mod ++ ".xml"),
    case file:rename(Old, New) of
        ok ->
            ok;
        {error, R} ->
            R1 = file:format_error(R),
            ?ABORT("could not rename file '~ts': ~s", [New, R1])
    end.

clean_up(Dir) ->
    ?DBG("cleaning up directory '~ts'", [Dir]),
    Files = ["packages-frame.html",
             "overview-summary.html",
             "modules-frame.html",
             "index.html", "erlang.png", "edoc-info"],
    foreach(fun(F) ->
                    File = filename:join(Dir, F),
                    ?DBG("deleting file '~ts'", [File]),
                    ok = file:delete(File)
            end, Files),
    ok.

make_doc_html(#{name := Name, html_target := Target, erl_docgen := Docgen} = Ctx) ->
    ?INFO("Installing html-files", []),
    ok = install_html_boilerplate(Ctx),
    ok = install_html_images(Ctx),
    Priv = filename:join([Docgen, "priv"]),
    Args = ["--noout",
            "--stringparam", "outdir", Target,
            "--stringparam", "docgen", Docgen,
            "--stringparam", "topdocdir", "doc",
            "--stringparam", "pdfdir", filename:join([Target,"pdf"]), %% why?
            "--xinclude",
            "--stringparam", "specs_file", maps:get(specs, Ctx),
            "--stringparam", "gendate", maps:get(date, Ctx),
            "--stringparam", "appname", Name,
            "--stringparam", "appver", maps:get(vsn, Ctx),
            "--stringparam", "extra_front_page_info", maps:get(front_page_info, Ctx, ""),
            "--stringparam", "stylesheet", "css/otp_doc.css",
            "--stringparam", "winprefix", "Erlang",
            "--stringparam", "logo", "images/erlang-logo.png",
            "--stringparam", "pdfname", maps:get(pdfname, Ctx, ""),
            "-path", filename:join([Priv, "dtd"]),
            "-path", filename:join([Priv, "dtd_html_entities"]),
            filename:join([Priv, "xsl", "db_html.xsl"]),
            "doc/src/book.xml"],
    ?DBG("generating html-files to '~ts'", [Target]),
    exec("xsltproc", Args).

make_doc_man(ManFiles, #{name := Name, man_target := Target, erl_docgen := Docgen} = Ctx) ->
    ?INFO("Installing man-files", []),
    Priv = filename:join([Docgen,"priv"]),
    Args0 = ["--stringparam", "company", maps:get(company, Ctx),
             "--stringparam", "docgen", Docgen,
             "--stringparam", "gendate", maps:get(date, Ctx),
             "--stringparam", "appname", Name,
             "--stringparam", "appver", maps:get(vsn, Ctx),
             "--xinclude",
             "-path", filename:join([Priv, "dtd"]),
             "-path", filename:join([Priv, "dtd_man_entities"]),
             filename:join([Priv, "xsl", "db_man.xsl"])],

    ManFile = fun ({Type, File}) ->
                       RootBase = filename:rootname(filename:basename(File)),
                       case Type of
                           man1 -> filename:join(["man1", RootBase ++ ".1"]);
                           man3 -> filename:join(["man3", RootBase ++ ".3"]);
                           man4 -> filename:join(["man4", RootBase ++ ".4"]);
                           man6 -> filename:join(["man6", RootBase ++ ".6"])
                       end
              end,

    foreach(fun ({_,File}=MF) ->
                    Dst = filename:join([Target, ManFile(MF)]),
                    ok = filelib:ensure_dir(Dst),
                    Args = ["--output", Dst] ++ Args0 ++ [File],
                    ?DBG("generating man-page '~ts'", [Dst]),
                    exec("xsltproc", Args)
            end, ManFiles),
    ok.

exec(Program, Args) ->
    ?DBG("executing '~ts' with ~p", [Program,Args]),
    case rebar3_otpdoc_system:run("xsltproc", Args) of
        {0,_} -> ok;
        {_,Error} ->
            ?ABORT("'~ts' failed with~n~ts~n", [Program,Error])
    end.

install_html_images(#{images := Files, html_target := Target, doc_src := DocSrc}) ->
    foreach(fun (File) ->
                    Src = filename:join(DocSrc, File),
                    Dst = filename:join([Target,File]),
                    ok = install_file(Src, Dst)
            end, Files),
    ok.

install_html_boilerplate(#{erl_docgen := Path, html_target := Target}) ->
    Files = ["js/flipmenu/flipmenu.js",
             "js/flipmenu/flip_closed.gif",
             "js/flipmenu/flip_open.gif",
             "js/flipmenu/flip_static.gif",
             "css/otp_doc.css",
             "images/erlang-logo.png"],
    foreach(fun (File) ->
                    Src = filename:join([Path,"priv",File]),
                    Dst = filename:join([Target,"doc",File]),
                    ok = install_file(Src, Dst)
            end, Files),
    ok.

install_file(Src, Dst) ->
    ok = filelib:ensure_dir(Dst),
    ?DBG("installing file '~ts' from '~ts'", [Dst,Src]),
    {ok,_} = file:copy(Src, Dst),
    ok.

%% include code examples

-define(match_codeinclude,
       "<codeinclude(?:\040|\t)*file=\"([^\"]*)\"(?:(?:(?:\040|\t)*tag=\"([^\"]*)\".*)|(?:.*))(?:/>|/codeinclude>)").
xml_codeline_preprocessing(InFile, OutFile) ->
    {ok, InFd} = file:open(InFile, [read]),
    Path = filename:dirname(InFile),
    {ok, OutFd} = file:open(OutFile, [write]),
    {ok, MP} = re:compile(?match_codeinclude),
    ok = xml_codeline_parse(InFd, OutFd, Path, MP),
    file:close(OutFd),
    file:close(InFd),
    ok.

xml_codeline_parse(InDev, OutDev, Path, Mp) ->
    case io:get_line(InDev, "") of
        eof ->
            ok;
        String ->
            case re:run(String, Mp,[{capture, [1,2], list}]) of
                {match,[File, []]} ->
                    Bin = read_example_or_abort(filename:join(Path, File)),
                    file:write(OutDev, "<code>\n<![CDATA[\n"),
                    file:write(OutDev, Bin),
                    file:write(OutDev, "]]></code>");
                {match,[File, Tag]} ->
                    Bin = read_example_or_abort(filename:join(Path, File)),
                    String2 = xml_codeline_get_code(Bin, Tag),
                    file:write(OutDev, "<code>\n<![CDATA[\n"),
                    file:write(OutDev, String2),
                    file:write(OutDev, "]]></code>");
                _ ->
                    file:write(OutDev, String)
            end,
            xml_codeline_parse(InDev, OutDev, Path, Mp)
    end.

read_example_or_abort(File) ->
    case file:read_file(File) of
        {ok,Bin} -> Bin;
        {error, enoent} ->
            ?ABORT("Could not find code-file to include: ~ts", [File])
    end.

xml_codeline_get_code(Bin, Tag) ->
    {match, [[Match]]} = re:run(Bin,"^" ++ Tag ++ "\n((.|\n)*)\n" ++ Tag ++ "\$",
                                [global, multiline, {capture, [1], binary}]),
    Match.

%% "some/xml-file.xml" -> man1 | man3 | man4 | man6 | none
classify_man_page(XmlFile) ->
    Event = fun({startDTD,Name,_,_}, _, _) ->
                    case Name of
                        "comref"      -> man1;
                        "cref"        -> man3;
                        "erlref"      -> man3;
                        "fileref"     -> man4;
                        "appref"      -> man6;
                        "chapter"     -> none;
                        "application" -> none;
                        "fascicules"  ->
                            ?WARN("This xml-file is deprecated and should be removed: '~ts'", [XmlFile]),
                            none;
                        "part"        -> none;
                        "book"        -> none
                    end;
               (_, _, S) ->
                    S
            end,
    case xmerl_sax_parser:file(XmlFile, [skip_external_dtd,
                                        {event_fun, Event},
                                        {event_state,none}]) of
        {ok, Type, _} ->
            Type;
        {fatal_error, _, Error, _, _}=Fatal ->
            ?DBG("xmerl_sax_parser returned with: ~p", [Fatal]),
            ?WARN("Could not classify xml-file '~ts' - ~p", [XmlFile,Error]),
            none
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
