%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools - beamcache
%%
%% Copyright (c) 2015 Konrad Zemek (konrad.zemek@gmail.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_beam_cache).

-include("rebar.hrl").

-export([compile/2]).


compile(File, Options) ->
    try
        CacheDir = os:getenv("BEAM_CACHE_DIR"),
        OutputGenerated = compile:output_generated(Options),
        case CacheDir =:= false orelse OutputGenerated =:= false of
            true -> compile:file(File, Options);
            false -> compile(File, Options, CacheDir)
        end
    catch
        Error:Reason ->
            ?WARN("Failed to use beamcache for file ~p with options ~p: ~p:~p~n",
                  [File, Options, Error, Reason]),
            ?WARN("~p~n", [erlang:get_stacktrace()]),
            compile:file(File, Options)
    end.

compile(File, Options, CacheDir) ->
    FilteredOptions =
        lists:filter(fun({outdir, _}) -> false; (_) -> true end, Options),

    SrcExtension = filename:extension(File),
    Basename = filename:basename(File, SrcExtension),

    CompilerInfo = compile:module_info(attributes),
    CompilerVersion = term_to_binary(proplists:get_value(vsn, CompilerInfo, "")),

    CompilerHash = base64url(CompilerVersion),
    OptionsHash = base64url(crypto:hash(md4, term_to_binary(FilteredOptions))),
    SrcDeps = get_deps(File, Options),
    FileHash = base64url(hash_files(SrcDeps)),

    CachePath = filename:join([CacheDir, CompilerHash, OptionsHash, Basename ++ FileHash]),
    OutDir = proplists:get_value(outdir, Options),
    OutPath = filename:join(OutDir, Basename ++ ".beam"),

    case filelib:is_regular(CachePath) of
        true ->
            ?INFO("Cache hit: ~p, path: ~p~n", [File, CachePath]),
            {ok, _} = file:copy(CachePath, OutPath),

            case filelib:is_regular(CachePath ++ ".warn") of
                false -> {ok, list_to_atom(Basename)};
                true ->
                    {ok, BinWarnings} = file:read_file(CachePath ++ ".warn"),
                    {ok, list_to_atom(Basename), binary_to_term(BinWarnings)}
            end;

        false ->
            case compile:file(File, Options) of
                {ok, ModuleName} ->
                    ok = filelib:ensure_dir(CachePath),
                    {ok, _} = file:copy(OutPath, CachePath),
                    {ok, ModuleName};

                {ok, ModuleName, Warnings} ->
                    BinWarnings = term_to_binary(Warnings),
                    ok = filelib:ensure_dir(CachePath),
                    ok = file:write_file(CachePath ++ ".warn", BinWarnings, [binary]),
                    {ok, _} = file:copy(OutPath, CachePath),
                    {ok, ModuleName, Warnings};

                Other -> Other
            end
    end.


get_deps(SrcPath, CompileOpts) ->
    CompileRes = compile:file(SrcPath, [makedep, binary | CompileOpts]),
    case element(1, CompileRes) of
        error ->
            ?WARN("Couldn't list deps of ~p: ~p~n", [SrcPath, CompileRes]),
            [SrcPath];
        _ ->
            [_, DepsBin] = binary:split(element(3, CompileRes), <<":">>),
            DepsBinList = re:split(DepsBin, <<"[\\s|\\\\]+">>, [trim]),
            [binary_to_list(D) || D <- DepsBinList, byte_size(D) > 0]
    end.


base64url(Bin) ->
    Base = base64:encode_to_string(Bin),
    lists:filtermap(fun
        ($+) -> {true, $-};
        ($/) -> {true, $_};
        ($=) -> false;
        (C) -> {true, C}
    end, Base).


hash_files(Paths) ->
    Context = crypto:hash_init(md4),
    hash_files(Paths, Context).

hash_files([], Context) ->
    crypto:hash_final(Context);
hash_files([Path | Paths], Context) ->
    {ok, File} = file:open(Path, [raw, binary, read]),
    hash_files(File, Paths, Context, {ok, <<>>}).

hash_files(File, Paths, Context, eof) ->
    file:close(File),
    hash_files(Paths, Context);
hash_files(File, Paths, Context, {ok, Data}) ->
    NewContext = crypto:hash_update(Context, Data),
    hash_files(File, Paths, NewContext, file:read(File, 64000)).
