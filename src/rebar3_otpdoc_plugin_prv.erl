-module(rebar3_otpdoc_plugin_prv).

-import(lists, [foreach/2]).

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
    foreach(fun(AppInfo) ->
                    make_doc_otp(AppInfo, rebar_state:resources(State))
            end, rebar_state:project_apps(State)),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%%-----------------------------------------------------------------
make_doc_otp(AppInfo, Resources) ->
    Opts = rebar_app_info:opts(AppInfo),
    OtpOpts = proplists:unfold(rebar_opts:get(Opts, otpdoc_opts, [])),
    OutDir = rebar_app_info:out_dir(AppInfo),
    DocSrc = filename:join([rebar_app_info:dir(AppInfo),"doc","src"]),
    io:format("Opts: ~p~n", [OtpOpts]),
    io:format("OutDir: ~p~n", [OutDir]),

    %% convert edoc modules to xml-files
    EdocMods = proplists:get_value(edoc_modules, OtpOpts, []),
    foreach(fun (Mod) ->
                    edoc_module_to_xml(filename:join(["src",Mod])++".erl", [])
            end, EdocMods),

    %% convert edoc chapters to xml-files
    EdocChapters = proplists:get_value(edoc_chapters, OtpOpts, []),
    foreach(fun (Chapter) ->
                    edoc_users_guide_to_xml(Chapter,[])
            end, EdocChapters),

    %% convert .xmlsrc-files to include code examples
    XmlSrcFiles = filelib:wildcard(filename:join(DocSrc, "*.xmlsrc")),
    foreach(fun (XmlSrcFile) ->
                    XmlDir = filename:dirname(XmlSrcFile),
                    Filename = filename:rootname(filename:basename(XmlSrcFile)),
                    XmlFileOut = filename:join([XmlDir,Filename]) ++ ".xml",
                    ok = xml_codeline_preprocessing(XmlSrcFile, XmlFileOut)
            end, XmlSrcFiles),

    %% generate specs
    ErlFiles = filelib:wildcard("src/*.erl"),
    HrlFiles = filelib:wildcard("{include,src}/*.hrl"),
    io:format("erl-files: ~p~n", [ErlFiles]),
    foreach(fun (ErlFile) ->
                    edoc_specs_to_xml(ErlFile,HrlFiles)
            end, ErlFiles),

    %% find defined man-pages
    ManFiles = case proplists:get_value(manpages, OtpOpts) of
                   undefined ->
                       XmlFiles = filelib:wildcard(filename:join(DocSrc, "*.xml")),
                       lists:foldl(fun(XmlFile, Acc) ->
                                           case classify_man_page(XmlFile) of
                                               none -> Acc;
                                               Type -> [{Type,XmlFile}|Acc]
                                           end
                                   end, [], XmlFiles);
                   XmlFiles ->
                       XmlFiles
               end,

    %% find doc-images
    Images = case proplists:get_value(images, OtpOpts) of
                 undefined ->
                     filelib:wildcard("**/*.{gif,jpeg,jpg,png}", DocSrc);
                 Imgs ->
                     Imgs
             end,

    %% set context
    Ctx = #{ name => rebar_app_info:name(AppInfo),
             vsn => rebar_utils:vcs_vsn(rebar_app_info:original_vsn(AppInfo),
                                        rebar_app_info:dir(AppInfo),
                                        Resources),
             specs => filename:join([DocSrc,"specs.xml"]),
             doc_src => DocSrc,
             images => Images,
             date => datestring(),
             dir => OutDir,
             erl_docgen => code:priv_dir(erl_docgen) },

    %% find xml-files
    ok = make_doc_html(Ctx),
    ok = make_doc_man(ManFiles, Ctx),
    ok.

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
            io:format("encoding: ~p~n", [Enc]),
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

edoc_specs_to_xml(File, InclFs) ->
    Dir = "doc/specs",
    ReadOpts = [{includes, InclFs}, {preprocess, true}],
    ExtractOpts = [{report_missing_type, false}],
    LayoutOpts = [{pretty_printer, erl_pp},
                  {layout, docgen_otp_specs}],
    try
        io:format("generate specs for ~ts (with ~p)~n", [File,InclFs]),
        Fs = clauses(read_file(File, ReadOpts)),
        Doc = extract(File, Fs, ExtractOpts),
        Text = edoc:layout(Doc, LayoutOpts),
        ok = write_text(Text, File, Dir),
        rename(Dir, File)
    catch
        _:_ ->
            io:format("EDoc could not process file '~s'\n", [File]),
            clean_up(Dir)
    end.

read_file(File, Opts) ->
    edoc:read_source(File, Opts).

extract(File, Forms, Opts) ->
    Env = edoc_lib:get_doc_env([], [], _Opts=[]),
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
    case file:write_file(OutFile, Text) of
        ok ->
            ok;
        {error, R} ->
            R1 = file:format_error(R),
            io:format("could not write file '~s': ~s\n", [File, R1])
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
            io:format("could not rename file '~s': ~s\n", [New, R1])
    end.

clean_up(Dir) ->
    _ = [file:delete(filename:join(Dir, F)) ||
            F <- ["packages-frame.html",
                  "overview-summary.html",
                  "modules-frame.html",
                  "index.html", "erlang.png", "edoc-info"]],
    ok.
make_doc_html(#{name := Name, erl_docgen := Priv} = Ctx) ->
    ok = install_html_boilerplate(Ctx),
    ok = install_html_images(Ctx),
    Args = ["--noout",
            "--stringparam", "outdir", "doc/html",
            "--stringparam", "docgen", "/ldisk/egil/git/otp/lib/erl_docgen",
            "--stringparam", "topdocdir", "doc",
            "--stringparam", "pdfdir", "doc/pdf",
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
    io:format("xsltproc ~p~n", [Args]),
    {0,_} = rebar3_otpdoc_system:run("xsltproc", Args),
    ok.

make_doc_man(ManFiles, #{name := Name, erl_docgen := Priv} = Ctx) ->
    Args0 = ["--stringparam", "company", "Ericsson AB",
             "--stringparam", "docgen", Priv,
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
                           man3 -> filename:join(["man3", RootBase ++ ".3"]);
                           man6 -> filename:join(["man6", RootBase ++ ".6"])
                       end
              end,

    foreach(fun ({_,File}=MF) ->
                    Args = ["--output", filename:join(["doc", ManFile(MF)])] ++ Args0 ++ [File],
                    {0,_} = rebar3_otpdoc_system:run("xsltproc", Args)
            end, ManFiles),
    ok.

install_html_images(#{images := Files, doc_src := DocSrc}) ->
    foreach(fun (File) ->
                    Src = filename:join(DocSrc, File),
                    Dst = filename:join(["doc/html", File]),
                    ok = install_file(Src, Dst)
            end, Files),
    ok.

install_html_boilerplate(#{erl_docgen := Path}) ->
    Files = ["js/flipmenu/flipmenu.js",
             "js/flipmenu/flip_closed.gif",
             "js/flipmenu/flip_open.gif",
             "js/flipmenu/flip_static.gif",
             "css/otp_doc.css",
             "images/erlang-logo.png"],
    foreach(fun (File) ->
                    Src = filename:join(Path,File),
                    Dst = filename:join(["doc/html/doc",File]),
                    ok = install_file(Src, Dst)
            end, Files),
    ok.

install_file(Src, Dst) ->
    ok = filelib:ensure_dir(Dst),
    io:format("copy: ~ts -> ~ts~n", [Src,Dst]),
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
                    {ok,Bin} = file:read_file(filename:join(Path, File)),
                    file:write(OutDev, "<code>\n<![CDATA[\n"),
                    file:write(OutDev, Bin),
                    file:write(OutDev, "]]></code>");
                {match,[File, Tag]} ->
                    String2 = xml_codeline_get_code(filename:join(Path, File), Tag),
                    file:write(OutDev, "<code>\n<![CDATA[\n"),
                    file:write(OutDev, String2),
                    file:write(OutDev, "]]></code>");
                _ ->
                    file:write(OutDev, String)
            end,
            xml_codeline_parse(InDev, OutDev, Path, Mp)
    end.

xml_codeline_get_code(File, Tag) ->
    {ok,Bin} = file:read_file(File),
    {match, [[Match]]} = re:run(Bin,"^" ++ Tag ++ "\n((.|\n)*)\n" ++ Tag ++ "\$",
                                [global, multiline, {capture, [1], binary}]),
    Match.

%% "some/xml-file.xml" -> man1 | man3 | man6 | none
classify_man_page(XmlFile) ->
    Event = fun({startDTD,Name,_,_}, _, _) ->
                    io:format("name: ~p~n", [Name]),
                    case Name of
                        "chapter"     -> man6;
                        "cref"        -> man3;
                        "erlref"      -> man3;
                        "comref"      -> man1;
                        "appref"      -> none; %% ?
                        "application" -> none;
                        "fascicules"  -> none;
                        "part"        -> none;
                        "book"        -> none
                    end;
               (_, _, S) ->
                    S
            end,
    {ok, Type, _} = xmerl_sax_parser:file(XmlFile, [skip_external_dtd,
                                                    {event_fun, Event},
                                                    {event_state,none}]),
    Type.


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
