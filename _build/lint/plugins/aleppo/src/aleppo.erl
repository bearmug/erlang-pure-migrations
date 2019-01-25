%%-------------------------------------------------------------------
%% @author
%%     ChicagoBoss Team and contributors
%% @end
%% @copyright
%%     This file is part of ChicagoBoss project.
%%     for license information, see LICENSE file in root directory
%% @end
%% @doc Aleppo: ALternative Erlang Pre-ProcessOr
%%-------------------------------------------------------------------

-module(aleppo).
-export([process_file/1, process_file/2, process_tokens/1, process_tokens/2,
         scan_file/1]).

-record(ale_context, {
        include_trail = [],
        include_dirs = [],
        macro_dict = dict:new()
    }).

process_file(FileName) ->
    process_file(FileName, []).

process_file(FileName, Options) ->
    ModuleName = list_to_atom(filename:rootname(filename:basename(FileName))),
    case scan_file(FileName) of
        {ok, Tokens} ->
            process_tokens(
                Tokens, [{file, FileName}, {module, ModuleName}|Options]);
        Error ->
            Error
    end.

process_tokens(Tokens) ->
    process_tokens(Tokens, []).

% Valid options:
% - file: The path of the file being processed
% - include: A list of directories to include in the .hrl search path
% - return_macros: return the macro dict in result if successful
process_tokens(Tokens, Options) ->
    {Tokens1, Module} = mark_keywords(Tokens),
    case aleppo_parser:parse(Tokens1) of
        {ok, ParseTree} ->
            process_tree(ParseTree, [{module, Module}|Options]);
        Error ->
            Error
    end.

process_tree(ParseTree, Options) ->
    {Dict0, IncludeTrail, IncludeDirs, TokenAcc} =
        case proplists:get_value(file, Options) of
            undefined -> {dict:new(), [], ["."], []};
            FileName -> {dict:store('FILE',
                                    [{string, 1, FileName}],
                                    dict:new()),
                         [filename:absname(FileName)],
                         [".", filename:dirname(FileName)],
                         lists:reverse(file_attribute_tokens(FileName, 1))}
        end,

    Dict1 = case proplists:get_value(module, Options) of
                undefined -> Dict0;
                Module ->
                    dict:store('MODULE', [{atom, 1, Module}],
                               dict:store('MODULE_STRING',
                                          [{string, 1, atom_to_list(Module)}],
                                          Dict0))
            end,

    Dict2 = dict:store('MACHINE',
                       [{atom, 1, list_to_atom(erlang:system_info(machine))}],
                       Dict1),

    Context = #ale_context{
        include_trail = IncludeTrail,
        include_dirs = IncludeDirs ++ proplists:get_value(include, Options, []),
        macro_dict = Dict2 },

    try process_tree(ParseTree, TokenAcc, Context) of
        {MacroDict, RevTokens} when is_list(RevTokens) ->
            FinalTokens = reverse_and_normalize_token_locations(RevTokens),
            case proplists:get_value(return_macros, Options, false) of
                true -> {ok, FinalTokens, MacroDict};
                _ -> {ok, FinalTokens}
            end
    catch
        _:Reason ->
            {error, Reason}
    end.

process_tree([], TokenAcc, Context) ->
    {Context#ale_context.macro_dict, TokenAcc};
process_tree([Node|Rest], TokenAcc, Context) ->
    try
        case Node of
            {'macro_define', {_Type, Loc, MacroName}} ->
                NewDict = dict:store(MacroName,
                                     [{atom, Loc, true}],
                                     Context#ale_context.macro_dict),
                process_tree(Rest,
                             TokenAcc,
                             Context#ale_context{ macro_dict = NewDict });
            {'macro_define', {_Type, _Loc, MacroName}, MacroTokens} ->
                NewDict = dict:store(MacroName,
                                 MacroTokens,
                                     Context#ale_context.macro_dict),
                process_tree(Rest,
                             TokenAcc,
                             Context#ale_context{ macro_dict = NewDict });
            {'macro_define',
             {_Type, _Loc, MacroName},
             MacroArgs, MacroTokens} ->
                NewDict = dict:store({MacroName, length(MacroArgs)},
                                     {MacroArgs, MacroTokens},
                                     Context#ale_context.macro_dict),
            process_tree(Rest,
                         TokenAcc,
                         Context#ale_context{ macro_dict = NewDict });
            {'macro_undef', {_Type, _Loc, MacroName}} ->
                NewDict = dict:erase(MacroName, Context#ale_context.macro_dict),
                process_tree(Rest,
                             TokenAcc,
                             Context#ale_context{ macro_dict = NewDict });
            {'macro_include', {string, Loc, FileName}} ->
                AbsName = expand_filename(FileName, Context),
                {NewDict, IncludeTokens} =
                    process_inclusion(AbsName, Loc, Context),
                process_tree(Rest,
                             IncludeTokens ++ TokenAcc,
                             Context#ale_context{ macro_dict = NewDict });
            {'macro_include_lib', {string, Loc, FileName}} ->
                AbsName = expand_include_lib(FileName),
                {NewDict, IncludeTokens} =
                    process_inclusion(AbsName, Loc, Context),
                process_tree(Rest,
                             IncludeTokens ++ TokenAcc,
                             Context#ale_context{ macro_dict = NewDict });
            {'macro_ifdef', {_Type, _Loc, MacroName}, IfBody} ->
                process_ifelse(Rest, MacroName, IfBody, [], TokenAcc, Context);
            {'macro_ifdef', {_Type, _Loc, MacroName}, IfBody, ElseBody} ->
                process_ifelse(Rest,
                               MacroName,
                               IfBody,
                               ElseBody,
                               TokenAcc,
                               Context);
            {'macro_ifndef', {_Type, _Loc, MacroName}, IfBody} ->
                process_ifelse(Rest, MacroName, [], IfBody, TokenAcc, Context);
            {'macro_ifndef', {_Type, _Loc, MacroName}, IfBody, ElseBody} ->
                process_ifelse(Rest,
                               MacroName,
                               ElseBody,
                               IfBody,
                               TokenAcc,
                               Context);
            {'macro', {var, {Line, _Col} = Loc, 'LINE'}} ->
                process_tree(Rest, [{integer, Loc, Line}|TokenAcc], Context);
            {'macro', {var, Line, 'LINE'}} when is_integer(Line) ->
                process_tree(Rest, [{integer, Line, Line}|TokenAcc], Context);
            {'macro', {var, Attrs, 'LINE'}} ->
                {Line, _} = location(Attrs),
                process_tree(Rest, [{integer, Attrs, Line}|TokenAcc], Context);
            {'macro', {_Type, Attrs, MacroName}} ->
                InsertTokens = dict:fetch(MacroName,
                                          Context#ale_context.macro_dict),
                {_, RevProcessedTokens} =
                    process_tree(InsertTokens, [], Context),

                LocFun = fun
                             ({T, _, V}) -> {T, Attrs, V};
                             (Token) -> Token
                         end,
                RevProcessedTokens1 = lists:map(LocFun, RevProcessedTokens),

                process_tree(Rest, RevProcessedTokens1 ++ TokenAcc, Context);
            {'macro', {_Type, Loc, MacroName}, MacroArgs} ->
                InsertTokens =
                    case dict:find({MacroName, length(MacroArgs)},
                                   Context#ale_context.macro_dict) of
                        {ok, {DefinedArgs, DefinedTokens}} ->
                            expand_macro_fun(Loc,
                                             DefinedArgs,
                                             DefinedTokens,
                                             MacroArgs);
                        _ ->
                            MacroArgsWithCommas =
                                insert_comma_tokens(MacroArgs, Loc),
                            dict:fetch(MacroName,
                                       Context#ale_context.macro_dict)
                                ++ [{'(', Loc}|MacroArgsWithCommas]
                                ++ [{')', Loc}]
                    end,
                {_, RevProcessedTokens} =
                    process_tree(InsertTokens, [], Context),
                process_tree(Rest, RevProcessedTokens ++ TokenAcc, Context);
            OtherToken ->
                process_tree(Rest, [OtherToken|TokenAcc], Context)
        end
    catch
        _:_ ->
            %% Get the actual token by unboxing it from aleppo's custom token
            ActualToken = erlang:element(2, Node),
            process_tree(Rest, [ActualToken|TokenAcc], Context)
    end.

process_ifelse(Rest, MacroName, IfBody, ElseBody, TokenAcc, Context) ->
    ChooseBody = case dict:is_key(MacroName, Context#ale_context.macro_dict) of
        true -> IfBody;
        false -> ElseBody
    end,
    {NewDict, NewTokens} = process_tree(ChooseBody, [], Context),
    process_tree(Rest,
                 NewTokens ++ TokenAcc,
                 Context#ale_context{ macro_dict = NewDict }).

process_inclusion(FileName, {Line, _}, Context) ->
    process_inclusion(FileName, Line, Context);
process_inclusion(FileName, Line, Context) ->
    case lists:member(FileName, Context#ale_context.include_trail) of
        true ->
            throw({error, {circular_inclusion, FileName}});
        false ->
            {ok, Tokens} = scan_file(FileName),
            {NewTokens, _} = mark_keywords(Tokens),
            case aleppo_parser:parse(NewTokens) of
                {ok, ParseTree} ->
                    [{eof, _}|Rest] = lists:reverse(ParseTree),
                    ParseTreeNoEOF = lists:reverse(Rest),
                    ThisFile =
                        case dict:find('FILE',
                                       Context#ale_context.macro_dict) of
                            {ok, Val} -> Val;
                            _ -> undefined
                        end,
                    Dict1 = dict:store('FILE',
                                       [{string, 1, FileName}],
                                       Context#ale_context.macro_dict),
                    TokenAcc = lists:reverse(file_attribute_tokens(FileName,
                                                                   1)),
                    {Dict2, IncludedTokens} =
                        process_tree(ParseTreeNoEOF,
                                     TokenAcc,
                                     Context#ale_context{
                                       macro_dict = Dict1,
                                       include_trail =
                                           [FileName
                                            |Context#ale_context.include_trail
                                           ]}),
                    case ThisFile of
                        undefined -> {Dict2, IncludedTokens};
                        [{string, _Loc, ThisFileName}] ->
                            {dict:store('FILE', ThisFile, Dict2),
                             lists:reverse(file_attribute_tokens(ThisFileName,
                                                                 Line))
                             ++ IncludedTokens}
                    end;
                Error ->
                    throw(Error)
            end
    end.

file_attribute_tokens(FileName, Line) ->
    [{'-', Line},
     {atom, Line, 'file'},
     {'(', Line},
     {string, Line, FileName},
     {',', Line},
     {integer, Line, Line},
     {')', Line},
     {dot, Line}].

expand_include_lib(FileName) ->
    [Lib | Rest] = filename:split(FileName),
    case code:lib_dir(list_to_atom(Lib)) of
        {error, _} ->
            throw({error, {not_found, FileName}});
        Result ->
            filename:join([Result|Rest])
    end.

expand_filename([$/|_] = FileName, _) ->
    case filelib:is_file(FileName) of
        true -> FileName;
        false -> throw({error, {not_found, FileName}})
    end;
expand_filename([$$|FileNameMinusDollar] = FileName, Context) ->
    [Var | Rest] = filename:split(FileNameMinusDollar),
    case os:getenv(Var) of
        false ->
            expand_relative_filename(FileName, Context);
        Value ->
            expand_filename(filename:join([Value|Rest]), Context)
    end;
expand_filename(FileName, Context) ->
    expand_relative_filename(FileName, Context).

expand_relative_filename(FileName, #ale_context{include_trail = IncTrail,
                                                include_dirs = IncDirs}) ->
    TrailDirs = [filename:dirname(Trail) || Trail <- IncTrail],
    ExpandedFileName = lists:foldl(
        fun
            (Dir, "") ->
                ExpandedFileName = filename:join(Dir, FileName),
                case filelib:is_file(ExpandedFileName) of
                    true -> ExpandedFileName;
                    false -> ""
                end;
            (_, F) ->
                F
        end, "", IncDirs ++ TrailDirs),
    case ExpandedFileName of
        "" -> throw({error, {not_found, FileName}});
        ExpandedFileName ->
            filename:absname(ExpandedFileName)
    end.

scan_file(FileName) ->
    {ok, FileContents} = file:read_file(FileName),
    Data = binary_to_list(FileContents),
    scan_tokens(Data).

scan_tokens(Data) ->
    scan_tokens(Data, {1, 1}).

scan_tokens(Data, StartLocation) ->
    case erl_scan:string(Data, StartLocation, [text]) of
        {ok, Tokens, _} ->
            {ok, Tokens};
        {error, ErrorInfo, _ErrorLocation} ->
            {error, ErrorInfo}
    end.

expand_macro_fun(Loc, DefinedArgs, DefinedTokens, ApplyArgs) ->
    ExpandedTokens = replace_macro_strings(DefinedTokens,
                                           DefinedArgs,
                                           ApplyArgs),
    DefinedArgsWithCommas = insert_comma_tokens(DefinedArgs, Loc),
    ApplyArgsWithCommas = insert_comma_tokens(ApplyArgs, Loc),
    [{'(', Loc}, {'fun', Loc}, {'(', Loc}|DefinedArgsWithCommas] ++
        [{')', Loc}, {'->', Loc}|ExpandedTokens] ++
        [{'end', Loc}, {')', Loc}, {'(', Loc}|ApplyArgsWithCommas] ++
        [{')', Loc}].

replace_macro_strings(DefinedTokens, DefinedArgs, ApplyArgs) ->
    Fun = fun([{var, _, VarName}], ApplyTokens) ->
                  ArgAsString = stringify_tokens(ApplyTokens),
                  {VarName, ArgAsString}
          end,
    MacroStringDict = dict:from_list(lists:zipwith(Fun,
                                                   DefinedArgs,
                                                   ApplyArgs)),
    replace_macro_strings1(DefinedTokens, MacroStringDict, []).

replace_macro_strings1([], _, Acc) ->
    lists:reverse(Acc);
replace_macro_strings1([{'macro_string', {var, Loc, VarName}}|Rest],
                       MacroStringDict,
                       Acc) ->
    replace_macro_strings1(Rest, MacroStringDict,
                           [{string, Loc, dict:fetch(VarName, MacroStringDict)}
                            |Acc]);
replace_macro_strings1([Token|Rest], MacroStringDict, Acc)
  when is_tuple(Token) ->
    Result0 = replace_macro_strings1(tuple_to_list(Token), MacroStringDict, []),
    Result1 = list_to_tuple(Result0),
    replace_macro_strings1(Rest, MacroStringDict, [Result1|Acc]);
replace_macro_strings1([Token|Rest], MacroStringDict, Acc)
  when is_list(Token) ->
    Result = replace_macro_strings1(Token, MacroStringDict, []),
    replace_macro_strings1(Rest, MacroStringDict, [Result|Acc]);
replace_macro_strings1([OtherToken|Rest], MacroStringDict, Acc) ->
    replace_macro_strings1(Rest, MacroStringDict, [OtherToken|Acc]).

stringify_tokens(TokenList) ->
    stringify_tokens1(TokenList, []).

stringify_tokens1([], Acc) ->
    lists:concat(lists:reverse(Acc));
stringify_tokens1([Token|Rest], []) ->
    Symbol = get_symbol(Token),
    stringify_tokens1(Rest, [Symbol]);
stringify_tokens1([Token|Rest], Acc) ->
    Symbol = get_symbol(Token),
    stringify_tokens1(Rest, [Symbol, " "|Acc]).

insert_comma_tokens(Args, Loc) ->
    lists:foldr(fun
            (Arg, []) -> Arg;
            (Arg, Acc) -> Arg ++ [{',', Loc}|Acc]
        end, [], Args).

mark_keywords(Tokens) ->
    mark_keywords(Tokens, undefined, []).

mark_keywords([], Module, Acc) ->
    {lists:reverse(Acc), Module};
mark_keywords([{'-', DashAttrs} = Dash,
               {atom, ModuleAttrs, 'module'} = Token,
               {'(', ParenAttrs} = Paren,
               {atom, ModNameAttrs, ModuleName} = ModToken|Rest],
              undefined,
              Acc) ->
    DashLoc = location(DashAttrs),
    ModuleLoc = location(ModuleAttrs),
    ParenLoc = location(ParenAttrs),
    ModNameLoc = location(ModNameAttrs),
    case {DashLoc, ModuleLoc, ParenLoc, ModNameLoc} of
        {{DashLine, 1},
         {DashLine, 2},
         {DashLine, _},
         {DashLine, _}} ->
            mark_keywords(Rest, ModuleName, [ModToken, Paren, Token, Dash|Acc]);
        _ ->
            mark_keywords([Token, Paren, ModToken|Rest], undefined, [Dash|Acc])
    end;
mark_keywords([{'-', DashAttrs} = Dash,
               {atom, AtomAttrs, Atom} = Token|Rest],
              Mod,
              Acc) ->
    DashLoc = location(DashAttrs),
    AtomLoc = location(AtomAttrs),
    case {DashLoc, AtomLoc} of
        {{DashLine, 1}, {DashLine, 2}} ->
            MarkedToken = case Atom of
                              'define' -> {define_keyword, AtomAttrs};
                              'ifdef' -> {ifdef_keyword, AtomAttrs};
                              'ifndef' -> {ifndef_keyword, AtomAttrs};
                              'else' -> {else_keyword, AtomAttrs};
                              'endif' -> {endif_keyword, AtomAttrs};
                              'undef' -> {undef_keyword, AtomAttrs};
                              'include' -> {include_keyword, AtomAttrs};
                              'include_lib' -> {include_lib_keyword, AtomAttrs};
                              _ -> Token
                          end,
            mark_keywords(Rest, Mod, [MarkedToken, Dash|Acc]);
        _ ->
            mark_keywords(Rest, Mod, [Token, Dash|Acc])
    end;
mark_keywords([Other|Rest], Mod, Acc) ->
    mark_keywords(Rest, Mod, [Other|Acc]).

location(Location = {_Line, _Column}) ->
    Location;
location(Attrs) ->
    location_helper(Attrs).

-ifdef(pre18).
location_helper(Attrs) ->
    legacy_location(Attrs).

get_symbol(Token) ->
    {symbol, Symbol} = erl_scan:token_info(Token, symbol),
    Symbol.
-else.
location_helper(Attrs) ->
    case erl_anno:is_anno(Attrs) of
        true ->
            erl_anno:location(Attrs);
        false ->
            legacy_location(Attrs)
    end.

get_symbol(Token) ->
    erl_scan:symbol(Token).
-endif.

legacy_location(Attrs) when is_list(Attrs) ->
    case proplists:get_value(location, Attrs) of
        {_, _} = Loc ->
            Loc;
        undefined ->
            Line = proplists:get_value(line, Attrs),
            Column = proplists:get_value(column, Attrs),
            {Line, Column}
    end.

attrs_normalized_location(Attrs, Location) ->
    Attrs1 = proplists:delete(line, Attrs),
    Attrs2 = proplists:delete(column, Attrs1),
    [{location, Location} | Attrs2].

reverse_and_normalize_token_locations(RevTokens) ->
    reverse_and_normalize_token_locations_helper(RevTokens, []).

reverse_and_normalize_token_locations_helper([], Acc) ->
    Acc;
reverse_and_normalize_token_locations_helper(
    [{Type, MaybeLocation} | Rest], Acc)
        when is_tuple(MaybeLocation) ->
    reverse_and_normalize_token_locations_helper(
        Rest, [{Type, location(MaybeLocation)}|Acc]);
reverse_and_normalize_token_locations_helper(
    [{Type, Attrs} | Rest], Acc)
        when is_list(Attrs) ->
    Location = location(Attrs),
    Attrs1 = attrs_normalized_location(Attrs, Location),
    reverse_and_normalize_token_locations_helper(
        Rest, [{Type, Attrs1} | Acc]);
reverse_and_normalize_token_locations_helper(
    [{Type, MaybeLocation, Extra} | Rest], Acc)
        when is_tuple(MaybeLocation) ->
    reverse_and_normalize_token_locations_helper(
        Rest, [{Type, location(MaybeLocation), Extra}|Acc]);
reverse_and_normalize_token_locations_helper(
    [{Type, Attrs, Extra} | Rest], Acc)
        when is_list(Attrs) ->
    Location = location(Attrs),
    Attrs1 = attrs_normalized_location(Attrs, Location),
    reverse_and_normalize_token_locations_helper(
        Rest, [{Type, Attrs1, Extra}|Acc]);
reverse_and_normalize_token_locations_helper([Other | Rest], Acc) ->
    reverse_and_normalize_token_locations_helper(Rest, [Other | Acc]).
