-module(rebar_beam_cache).

-include("rebar.hrl").

-export([compile/2]).


compile(File, Options) ->
    CacheDir = os:getenv("BEAM_CACHE_DIR"),
    compile(File, Options, CacheDir).


compile(File, Options, false = _CacheDir) ->
    compile:file(File, Options);

compile(File, Options, CacheDir) ->
    FilteredOptions = lists:filter(fun
        (report_errors) -> false;
        (report_warnings) -> false;
        (report) -> false;
        (return_warnings) -> false;
        (verbose) -> false;
        ({outdir, _}) -> false;
        ({warn_format, _}) -> false;
        (_) -> true
    end, Options),

    SrcExtension = filename:extension(File),
    Basename = filename:basename(File, SrcExtension),

    CompilerInfo = compile:module_info(attributes),
    CompilerVersion = term_to_binary(proplists:get_value(vsn, CompilerInfo, "")),

    CompilerHash = base64url(CompilerVersion),
    OptionsHash = base64url(crypto:hash(md4, term_to_binary(FilteredOptions))),
    FileHash = base64url(hash_file(File)),

    CachePath = filename:join([CacheDir, CompilerHash, OptionsHash, Basename ++ FileHash]),
    OutDir = proplists:get_value(outdir, Options),
    OutPath = filename:join(OutDir, Basename ++ ".beam"),

    case filelib:is_regular(CachePath) of
        true ->
            ?INFO("Cache hit: ~p, path: ~p~n", [File, CachePath]),
            {ok, _} = file:copy(CachePath, OutPath),
            {ok, list_to_atom(Basename)};

        false ->
            case compile:file(File, Options) of
                {ok, ModuleName} ->
                    cache(CachePath, OutPath),
                    {ok, ModuleName};

                {ok, ModuleName, Warnings} ->
                    cache(CachePath, OutPath),
                    {ok, ModuleName, Warnings};

                Other -> Other
            end
    end.


cache(CachePath, OutPath) ->
    ok = filelib:ensure_dir(CachePath),
    {ok, _} = file:copy(OutPath, CachePath).


base64url(Bin) ->
    Base = base64:encode_to_string(Bin),
    lists:filtermap(fun
        ($+) -> {true, $-};
        ($/) -> {true, $_};
        ($=) -> false;
        (C) -> {true, C}
    end, Base).


hash_file(Path) ->
    Context = crypto:hash_init(md4),
    {ok, File} = file:open(Path, [raw, binary, read]),
    hash_file(File, Context).

hash_file(File, Context) ->
    hash_file(File, Context, {ok, <<>>}).

hash_file(File, Context, eof) ->
    file:close(File),
    crypto:hash_final(Context);

hash_file(File, Context, {ok, Data}) ->
    NewContext = crypto:hash_update(Context, Data),
    hash_file(File, NewContext, file:read(File, 64000)).
