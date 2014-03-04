% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(knit_overlay).


-export([
    run/0
]).


-include("knit.hrl").


run() ->
    OverlayVars = load_overlay_vars(),

    Render = fun(Template) ->
        knit_render:run(Template, OverlayVars)
    end,

    case knit_cfg:get(overlay) of
        undefined ->
            knit_log:debug("No {overlay, [...]} to process.");
        Overlay when is_list(Overlay) ->
            execute_overlay(Overlay, Render);
        Overlay ->
            Fmt = "Invalid overlay entry in reltool.config:~n  ~p",
            ?BAD_CONFIG(Fmt, [Overlay])
    end.


load_overlay_vars() ->
    {_, BootRelVsn} = knit_vsn:boot_rel(),

    BaseVars = dict:from_list([
        {erts_vsn, "erts-" ++ erlang:system_info(version)},
        {rel_vsn, BootRelVsn},
        {target_dir, knit_cfg:get(target_dir)},
        {build_dir, knit_cfg:get(build_dir)},
        {hostname, net_adm:localhost()}
    ]),

    FileNames = case knit_cfg:get(overlay_vars) of
        undefined -> [];
        OV -> [OV]
    end ++ case knit_cfg:get(extra_overlay_vars) of
        undefined -> [];
        EOV -> [EOV]
    end,

    OverlayVars = lists:foldl(fun(FileName, Vars) ->
        lists:foldl(fun({K, V}, Acc) ->
            dict:store(K, V, Acc)
        end, Vars, knit_file:consult(FileName))
    end, BaseVars, FileNames),

    knit_render:resolve(OverlayVars).


execute_overlay(Commands, Render) ->
    BaseDir = knit_file:cwd(),
    BuildDir = knit_cfg:get(build_dir),

    lists:foreach(fun(C) ->
        execute_command(C, Render, BaseDir, BuildDir)
    end, Commands).


execute_command({mkdir, Out}, Render, _, BuildDir) ->
    OutDir = Render(filename:join([BuildDir, Out])),
    knit_log:debug("overlay: mkdir ~s", [Out]),
    ok = knit_file:ensure_dir_exists(OutDir);

execute_command({copy, In}, Render, BaseDir, BuildDir) ->
    execute_command({copy, In, ""}, Render, BaseDir, BuildDir);

execute_command({copy, In, Out}, Render, BaseDir, BuildDir) ->
    InFile = Render(filename:join(BaseDir, In)),
    OutFile = Render(filename:join(BuildDir, Out)),
    case filelib:is_dir(InFile) of
        true ->
            ok;
        false ->
            ok = filelib:ensure_dir(OutFile)
    end,
    knit_log:debug("overlay: cp ~s ~s", [In, Out]),
    knit_file:cp_r([InFile], OutFile);

execute_command({template_wildcard, Wildcard, OutDir},
        Render, BaseDir, BuildDir) ->

    Files = filelib:wildcard(Wildcard, BaseDir),

    if length(Files) > 0 -> ok; true ->
        Fmt = "template wildcard did not match any files: ~s",
        knit_debug:warn(Fmt, [Wildcard])
    end,

    FileStr = string:join(Files, ", "),
    knit_log:debug("overlay: template wildcard ~s -> ~s", [Wildcard, FileStr]),

    lists:foreach(fun(File) ->
        OutFile = filename:join(OutDir, filename:basename(File)),
        Cmd = {template, File, OutFile},
        execute_command(Cmd, Render, BaseDir, BuildDir)
    end, Files);

execute_command({template, In, Out}, Render, BaseDir, BuildDir) ->
    InFile = Render(filename:join(BaseDir, In)),
    OutFile = Render(filename:join(BuildDir, Out)),

    {ok, InFileData} = file:read_file(InFile),
    ok = filelib:ensure_dir(OutFile),

    case file:write_file(OutFile, Render(InFileData)) of
        ok ->
            ok = knit_file:copy_file_info(InFile, OutFile),
            knit_log:debug("overlay: template ~s ~s", [In, Out]);
        {error, Reason} ->
            Fmt = "Error writing to ~s : ~s",
            ?IO_ERROR(Fmt, [OutFile, file:format_error(Reason)])
    end;

execute_command({create, Out, Contents}, Render, _, BuildDir) ->
    OutFile = Render(filename:join(BuildDir, Out)),
    ok = filelib:ensure_dir(OutFile),

    case file:write_file(OutFile, Contents) of
        ok ->
            knit_log:debug("overlay: create ~s", [Out]);
        {error, Reason} ->
            Fmt = "Error creating ~s: ~s",
            ?IO_ERROR(Fmt, [OutFile, file:format_error(Reason)])
    end;

execute_command({replace, Out, RegX, Repl}, Render, BaseDir, BuildDir) ->
    execute_command({replace, Out, RegX, Repl, []}, Render, BaseDir, BuildDir);

execute_command({replace, Out, RegX, Repl, Opts}, Render, _, BuildDir) ->
    OutFile = Render(filename:join(BuildDir, Out)),

    {ok, OrigData} = file:read_file(OutFile),
    ReOpts = [global, {return, binary}] ++ Opts,
    NewData = re:replace(OrigData, RegX, Render(Repl), ReOpts),

    case file:write_file(OutFile, NewData) of
        ok ->
            knit_log:debug("replace ~s: s/~s/~s/", [Out, RegX, Repl]);
        {error, Reason} ->
            Fmt = "Failed to replace ~s: ~s",
            ?IO_ERROR(Fmt, [OutFile, file:format_error(Reason)])
    end;

execute_command(Unknown, _Render, _BaseDir, _BuildDir) ->
    Fmt = "Unsupported overlay command: ~w",
    ?BAD_CONFIG(Fmt, [Unknown]).

