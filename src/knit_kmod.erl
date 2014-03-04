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

-module(knit_kmod).


-export([
    render/1
]).


-include("knit.hrl").


% Module attributes:
% knit_priority - Set a module priority - int() | default (which is 0)
% knit_extra - Set the value of Extra for code_change
% knit_depends - Set module dependencies - [atom()]
% knit_timeout - Set an upgrade timeout - int()>0 | default | infinity
% knit_purge - Set the purge style - Purge | {PrePurge, PostPerge}
% knit_apply - Run a function during upgrade [MFA | {Phase, MFA}]
%              where phase is first | immediate | last
%              or {phase, priority}
%
%
% High-level
% ==========
%
% {update, Mod}
% {update, Mod, supervisor}
% {update, Mod, Change}
% {update, Mod, DepMods}
% {update, Mod, Change, DepMods}
% {update, Mod, Change, PrePurge, PostPurge, DepMods}
% {update, Mod, Timeout, Change, PrePurge, PostPurge, DepMods}
% {update, Mod, ModType, Timeout, Change, PrePurge, PostPurge, DepMods}
% Mod = atom()
% ModType = static | dynamic
% Timeout = int()>0 | default | infinity
% Change = soft | {advanced,Extra}
%   Extra = term()
% PrePurge = PostPurge = soft_purge | brutal_purge
% DepMods = [Mod]
%
% {load_module, Mod}
% {load_module, Mod, DepMods}
% {load_module, Mod, PrePurge, PostPurge, [Mod]}
% Mod = atom()
% PrePurge = PostPurge = soft_purge | brutal_purge
% DepMods = [Mod]
%
% {add_module, Mod}
%   Mod = atom()
%
% {delete_module, Mod}
%   Mod = atom()
%
% {restart_application, Application}
%   Application = atom()


-record(kmod, {
    name,
    action,
    behaviors,
    has_code_change,
    priority,
    extra,
    depends,
    timeout,
    purge,
    apply_funs
}).


render(BeamActions) ->
    KMods0 = lists:map(fun new_kmod/1, BeamActions),
    KMods1 = lists:sort(fun cmp_kmods/2, KMods0),
    Instructions = lists:flatmap(fun render_kmod/1, KMods1),
    FirstFuns = render_apply_funs(first, KMods1),
    LastFuns = render_apply_funs(last, KMods1),
    FirstFuns ++ Instructions ++ LastFuns.


new_kmod({added, BeamFile}) ->
    new_kmod(add, BeamFile);
new_kmod({changed, {_OldBeamFile, NewBeamFile}}) ->
    new_kmod(update, NewBeamFile);
new_kmod({removed, BeamFile}) ->
    Name = filename:rootname(filename:basename(BeamFile)),
    #kmod{name=list_to_atom(Name), action=delete}.


new_kmod(Action, BeamFile) ->
    {ok, {Name, Chunks}} = beam_lib:chunks(BeamFile, [attributes, exports]),
    [{attributes, Attrs}, {exports, Exports}] = lists:sort(Chunks),
    #kmod{
        name = Name,
        action = Action,
        behaviors = get_behaviors(Name, Attrs),
        has_code_change = lists:keymember(code_change, 1, Exports),
        priority = get_priority(Name, Attrs),
        extra = get_extra(Name, Attrs),
        depends = get_depends(Name, Attrs),
        timeout = get_timeout(Name, Attrs),
        purge = get_purge(Name, Attrs),
        apply_funs = get_apply_funs(Name, Attrs)
    }.


cmp_kmods(A0, B0) ->
    % Order by action (add, then update, then delete)
    % then secondary sort on priority. The negation on
    % priority is so that higher priorities run first.
    ActOrder = fun
        (#kmod{action=add}) -> -1;
        (#kmod{action=update}) -> 0;
        (#kmod{action=delete}) -> 1
    end,
    A = {ActOrder(A0), -1 * A0#kmod.priority},
    B = {ActOrder(B0), -1 * B0#kmod.priority},
    A =< B.


render_kmod(#kmod{action=add}=K) ->
    [{add_module, K#kmod.name}];
render_kmod(#kmod{action=delete}=K) ->
    [{delete_module, K#kmod.name}];
render_kmod(#kmod{action=update}=K) ->
    #kmod{
        name = Name,
        behaviors = Behaviors,
        has_code_change = HasCC,
        extra = Extra0,
        depends = Deps,
        timeout = Timeout,
        purge = {Pre, Post}
    } = K,
    IsSup = lists:member(supervisor, Behaviors),
    Instructions0 = case {IsSup, HasCC} of
        {true, _} ->
            [{update, Name, static, Timeout, {advanced, []}, Pre, Post, Deps}];
        {false, true} ->
            Extra = case Extra0 of
                soft -> {advanced, []};
                _ -> Extra0
            end,
            [{update, Name, dynamic, Timeout, Extra, Pre, Post, Deps}];
        {false, false} ->
            [{load_module, Name, Pre, Post, Deps}]
    end,
    Instructions0 ++ render_apply_funs(immediate, [K]).


render_apply_funs(Phase, KMods) ->
    Funs0 = lists:flatmap(fun(K) ->
        lists:flatmap(fun({{Phase0, Pri}, MFA}) ->
            if Phase0 /= Phase -> []; true ->
                [{Pri, MFA}]
            end
        end, K#kmod.apply_funs)
    end, KMods),
    Funs1 = lists:sort(Funs0),
    [{apply, MFA} || {_, MFA} <- Funs1].


get_behaviors(_Name, Attrs) ->
    case get_attr(behavior, Attrs) of
        Behaviors0 when is_list(Behaviors0) ->
            Behaviors0;
        undefined ->
            case get_attr(behaviour, Attrs) of
                Behaviours0 when is_list(Behaviours0) ->
                    Behaviours0;
                undefined ->
                    []
            end
    end.


get_priority(Name, Attrs) ->
    case get_attr(knit_priority, Attrs) of
        [P] when is_integer(P) ->
            P;
        undefined ->
            0;
        Else ->
            ?BAD_CONFIG("Invalid knit_priority in ~s: ~p", [Name, Else])
    end.


get_extra(Name, Attrs) ->
    case get_attr(knit_extra, Attrs) of
        [E] ->
            {advanced, E};
        undefined ->
            soft;
        Else ->
            ?BAD_CONFIG("Invalid knit_extra in ~s: ~p", [Name, Else])
    end.


get_depends(Name, Attrs) ->
    case get_attr(knit_depends, Attrs) of
        Modules0 when is_list(Modules0) ->
            Modules = lists:flatten(Modules0),
            lists:foreach(fun(M) ->
                case is_atom(M) of
                    true ->
                        ok;
                    false ->
                        ?BAD_CONFIG("Invalid dependency in ~s: ~p", [Name, M])
                end
            end, Modules),
            Modules;
        undefined ->
            []
    end.


get_timeout(Name, Attrs) ->
    case get_attr(knit_timeout, Attrs) of
        [default] ->
            default;
        [infinity] ->
            infinity;
        [TO] when is_integer(TO), TO >= 0 ->
            TO;
        undefined ->
            default;
        Else ->
            ?BAD_CONFIG("Invalid timeout in ~s: ~p", [Name, Else])
    end.


get_purge(Name, Attrs) ->
    case get_attr(knit_purge, Attrs) of
        [soft_purge] ->
            {soft_purge, soft_purge};
        [brutal_purge] ->
            {brutal_purge, brutal_purge};
        [{Pre, Post}] when
                (Pre == soft_purge orelse Pre == brutal_purge),
                (Post == soft_purge orelse Post == brutal_purge) ->
            {Pre, Post};
        undefined ->
            {brutal_purge, brutal_purge};
        Else ->
            ?BAD_CONFIG("Invalid purge in ~s: ~p", [Name, Else])
    end.


get_apply_funs(Name, Attrs) ->
    case get_attr(knit_apply, Attrs) of
        undefined ->
            [];
        Funs ->
            lists:map(fun(F) -> validate_apply_fun(Name, F) end, Funs)
    end.


validate_apply_fun(Name, {_, _, _}=MFA) ->
    {{last, 0}, validate_mfa(Name, MFA)};
validate_apply_fun(Name, {Phase, MFA}) ->
    {validate_phase(Name, Phase), validate_mfa(Name, MFA)}.


validate_mfa(_Name, {Mod, Fun, Args})
        when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    {Mod, Fun, Args};
validate_mfa(Name, BadMFA) ->
    ?BAD_CONFIG("Invalid apply MFA in ~s: ~p", [Name, BadMFA]).


validate_phase(_Name, first) ->
    {first, 0};
validate_phase(_Name, {first, P}) when is_integer(P) ->
    {first, P};
validate_phase(_Name, immediate) ->
    {immediate, 0};
validate_phase(_Name, {immediate, P}) when is_integer(P) ->
    {immediate, P};
validate_phase(_Name, last) ->
    {last, 0};
validate_phase(_Name, {last, P}) when is_integer(P) ->
    {last, P};
validate_phase(Name, BadPhase) ->
    ?BAD_CONFIG("Invalid apply phase in ~s: ~p", [Name, BadPhase]).


get_attr(Name, Attrs) ->
    case lists:keyfind(Name, 1, Attrs) of
        {Name, Value} ->
            Value;
        false ->
            undefined
    end.
