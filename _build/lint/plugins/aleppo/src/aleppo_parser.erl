-module(aleppo_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).

-file("/usr/lib/erlang/lib/parsetools-2.1.8/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error: Stacktrace ->
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) -> [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write_atom(Other);
yecctoken2string(Other) ->
    io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.erl", 176).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_62(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_2(2, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_1/7}).
yeccpars2_1(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_1(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_2(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '...', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, callback, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, comment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, eof, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, spec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 'yeccgoto_\'File\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_3_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_4_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_5_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_6_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_7_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccpars2_169(180, Cat, [8 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_9_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2_169(169, Cat, [10 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_11_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_47/7}).
yeccpars2_47(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_47(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_62(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_67_(Stack),
 'yeccgoto_\'Elements\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_69_(Stack),
 'yeccgoto_\'File\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_90_(Stack),
 'yeccgoto_\'Macro\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroName\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroName\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_93(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'!\''(Stack),
 yeccpars2_96(96, '!', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'#\''(Stack),
 yeccpars2_96(96, '#', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'(\''(Stack),
 yeccpars2_96(96, '(', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'*\''(Stack),
 yeccpars2_96(96, '*', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'+\''(Stack),
 yeccpars2_96(96, '+', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'++\''(Stack),
 yeccpars2_96(96, '++', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'-\''(Stack),
 yeccpars2_96(96, '-', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'--\''(Stack),
 yeccpars2_96(96, '--', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'->\''(Stack),
 yeccpars2_96(96, '->', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'.\''(Stack),
 yeccpars2_96(96, '.', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'..\''(Stack),
 yeccpars2_96(96, '..', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'...\''(Stack),
 yeccpars2_96(96, '...', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'/\''(Stack),
 yeccpars2_96(96, '/', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'/=\''(Stack),
 yeccpars2_96(96, '/=', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\':\''(Stack),
 yeccpars2_96(96, ':', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'::\''(Stack),
 yeccpars2_96(96, '::', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\':=\''(Stack),
 yeccpars2_96(96, ':=', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\';\''(Stack),
 yeccpars2_96(96, ';', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'<\''(Stack),
 yeccpars2_96(96, '<', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'<-\''(Stack),
 yeccpars2_96(96, '<-', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'<<\''(Stack),
 yeccpars2_96(96, '<<', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'<=\''(Stack),
 yeccpars2_96(96, '<=', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'=\''(Stack),
 yeccpars2_96(96, '=', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'=/=\''(Stack),
 yeccpars2_96(96, '=/=', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'=:=\''(Stack),
 yeccpars2_96(96, '=:=', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'=<\''(Stack),
 yeccpars2_96(96, '=<', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'==\''(Stack),
 yeccpars2_96(96, '==', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'=>\''(Stack),
 yeccpars2_96(96, '=>', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'>\''(Stack),
 yeccpars2_96(96, '>', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'>=\''(Stack),
 yeccpars2_96(96, '>=', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'>>\''(Stack),
 yeccpars2_96(96, '>>', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'?\''(Stack),
 yeccpars2_96(96, '?', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'[\''(Stack),
 yeccpars2_96(96, '[', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'after\''(Stack),
 yeccpars2_96(96, 'after', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'and\''(Stack),
 yeccpars2_96(96, 'and', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'andalso\''(Stack),
 yeccpars2_96(96, 'andalso', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_atom(Stack),
 yeccpars2_96(96, atom, [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'band\''(Stack),
 yeccpars2_96(96, 'band', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'begin\''(Stack),
 yeccpars2_96(96, 'begin', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'bnot\''(Stack),
 yeccpars2_96(96, 'bnot', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'bor\''(Stack),
 yeccpars2_96(96, 'bor', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'bsl\''(Stack),
 yeccpars2_96(96, 'bsl', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'bsr\''(Stack),
 yeccpars2_96(96, 'bsr', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'bxor\''(Stack),
 yeccpars2_96(96, 'bxor', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, callback, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_callback(Stack),
 yeccpars2_96(96, callback, [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'case\''(Stack),
 yeccpars2_96(96, 'case', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'catch\''(Stack),
 yeccpars2_96(96, 'catch', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_char(Stack),
 yeccpars2_96(96, char, [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_comment(Stack),
 yeccpars2_96(96, comment, [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'div\''(Stack),
 yeccpars2_96(96, 'div', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'end\''(Stack),
 yeccpars2_96(96, 'end', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_float(Stack),
 yeccpars2_96(96, float, [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'fun\''(Stack),
 yeccpars2_96(96, 'fun', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'if\''(Stack),
 yeccpars2_96(96, 'if', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_integer(Stack),
 yeccpars2_96(96, integer, [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'not\''(Stack),
 yeccpars2_96(96, 'not', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'of\''(Stack),
 yeccpars2_96(96, 'of', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'or\''(Stack),
 yeccpars2_96(96, 'or', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'orelse\''(Stack),
 yeccpars2_96(96, 'orelse', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'receive\''(Stack),
 yeccpars2_96(96, 'receive', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'rem\''(Stack),
 yeccpars2_96(96, 'rem', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_spec(Stack),
 yeccpars2_96(96, spec, [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_string(Stack),
 yeccpars2_96(96, string, [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'try\''(Stack),
 yeccpars2_96(96, 'try', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_var(Stack),
 yeccpars2_96(96, var, [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'when\''(Stack),
 yeccpars2_96(96, 'when', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'xor\''(Stack),
 yeccpars2_96(96, 'xor', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'{\''(Stack),
 yeccpars2_96(96, '{', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'|\''(Stack),
 yeccpars2_96(96, '|', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_93_\'||\''(Stack),
 yeccpars2_96(96, '||', [93 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 yeccpars2_97(97, Cat, [93 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_94(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '..', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '...', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':=', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=>', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, callback, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), callback, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), comment, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 'yeccgoto_\'NonEmptyApplyMacroArgs\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_95(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ApplyMacroArgs\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_96(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_96/7}).
yeccpars2_cont_96(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '++', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '...', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '::', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '<-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '<<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '=<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '==', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '=>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '>>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'and', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'band', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, callback, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, comment, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'div', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'if', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'not', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'or', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, spec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 88, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_96(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_97/7}).
yeccpars2_97(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 98, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 'yeccgoto_\'Macro\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_99_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_101_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_102(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'!\''(Stack),
 yeccpars2_96(96, '!', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'#\''(Stack),
 yeccpars2_96(96, '#', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'(\''(Stack),
 yeccpars2_96(96, '(', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'*\''(Stack),
 yeccpars2_96(96, '*', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'+\''(Stack),
 yeccpars2_96(96, '+', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'++\''(Stack),
 yeccpars2_96(96, '++', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'-\''(Stack),
 yeccpars2_96(96, '-', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'--\''(Stack),
 yeccpars2_96(96, '--', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'->\''(Stack),
 yeccpars2_96(96, '->', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'.\''(Stack),
 yeccpars2_96(96, '.', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'..\''(Stack),
 yeccpars2_96(96, '..', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'...\''(Stack),
 yeccpars2_96(96, '...', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'/\''(Stack),
 yeccpars2_96(96, '/', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'/=\''(Stack),
 yeccpars2_96(96, '/=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\':\''(Stack),
 yeccpars2_96(96, ':', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'::\''(Stack),
 yeccpars2_96(96, '::', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\':=\''(Stack),
 yeccpars2_96(96, ':=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\';\''(Stack),
 yeccpars2_96(96, ';', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'<\''(Stack),
 yeccpars2_96(96, '<', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'<-\''(Stack),
 yeccpars2_96(96, '<-', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'<<\''(Stack),
 yeccpars2_96(96, '<<', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'<=\''(Stack),
 yeccpars2_96(96, '<=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'=\''(Stack),
 yeccpars2_96(96, '=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'=/=\''(Stack),
 yeccpars2_96(96, '=/=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'=:=\''(Stack),
 yeccpars2_96(96, '=:=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'=<\''(Stack),
 yeccpars2_96(96, '=<', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'==\''(Stack),
 yeccpars2_96(96, '==', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'=>\''(Stack),
 yeccpars2_96(96, '=>', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'>\''(Stack),
 yeccpars2_96(96, '>', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'>=\''(Stack),
 yeccpars2_96(96, '>=', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'>>\''(Stack),
 yeccpars2_96(96, '>>', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'?\''(Stack),
 yeccpars2_96(96, '?', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'[\''(Stack),
 yeccpars2_96(96, '[', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'after\''(Stack),
 yeccpars2_96(96, 'after', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'and\''(Stack),
 yeccpars2_96(96, 'and', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'andalso\''(Stack),
 yeccpars2_96(96, 'andalso', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_atom(Stack),
 yeccpars2_96(96, atom, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'band\''(Stack),
 yeccpars2_96(96, 'band', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'begin\''(Stack),
 yeccpars2_96(96, 'begin', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'bnot\''(Stack),
 yeccpars2_96(96, 'bnot', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'bor\''(Stack),
 yeccpars2_96(96, 'bor', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'bsl\''(Stack),
 yeccpars2_96(96, 'bsl', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'bsr\''(Stack),
 yeccpars2_96(96, 'bsr', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'bxor\''(Stack),
 yeccpars2_96(96, 'bxor', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, callback, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_callback(Stack),
 yeccpars2_96(96, callback, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'case\''(Stack),
 yeccpars2_96(96, 'case', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'catch\''(Stack),
 yeccpars2_96(96, 'catch', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_char(Stack),
 yeccpars2_96(96, char, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_comment(Stack),
 yeccpars2_96(96, comment, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'div\''(Stack),
 yeccpars2_96(96, 'div', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'end\''(Stack),
 yeccpars2_96(96, 'end', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_float(Stack),
 yeccpars2_96(96, float, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'fun\''(Stack),
 yeccpars2_96(96, 'fun', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'if\''(Stack),
 yeccpars2_96(96, 'if', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_integer(Stack),
 yeccpars2_96(96, integer, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'not\''(Stack),
 yeccpars2_96(96, 'not', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'of\''(Stack),
 yeccpars2_96(96, 'of', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'or\''(Stack),
 yeccpars2_96(96, 'or', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'orelse\''(Stack),
 yeccpars2_96(96, 'orelse', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'receive\''(Stack),
 yeccpars2_96(96, 'receive', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'rem\''(Stack),
 yeccpars2_96(96, 'rem', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_spec(Stack),
 yeccpars2_96(96, spec, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_string(Stack),
 yeccpars2_96(96, string, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'try\''(Stack),
 yeccpars2_96(96, 'try', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_var(Stack),
 yeccpars2_96(96, var, [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'when\''(Stack),
 yeccpars2_96(96, 'when', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'xor\''(Stack),
 yeccpars2_96(96, 'xor', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'{\''(Stack),
 yeccpars2_96(96, '{', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'|\''(Stack),
 yeccpars2_96(96, '|', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_102_\'||\''(Stack),
 yeccpars2_96(96, '||', [102 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccpars2_117(117, Cat, [102 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_104(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_105(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'!\''(Stack),
 yeccpars2_96(96, '!', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'#\''(Stack),
 yeccpars2_96(96, '#', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'(\''(Stack),
 yeccpars2_96(96, '(', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'*\''(Stack),
 yeccpars2_96(96, '*', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'+\''(Stack),
 yeccpars2_96(96, '+', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'++\''(Stack),
 yeccpars2_96(96, '++', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'-\''(Stack),
 yeccpars2_96(96, '-', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'--\''(Stack),
 yeccpars2_96(96, '--', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'->\''(Stack),
 yeccpars2_96(96, '->', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'.\''(Stack),
 yeccpars2_96(96, '.', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'..\''(Stack),
 yeccpars2_96(96, '..', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'...\''(Stack),
 yeccpars2_96(96, '...', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'/\''(Stack),
 yeccpars2_96(96, '/', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'/=\''(Stack),
 yeccpars2_96(96, '/=', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\':\''(Stack),
 yeccpars2_96(96, ':', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'::\''(Stack),
 yeccpars2_96(96, '::', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\':=\''(Stack),
 yeccpars2_96(96, ':=', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\';\''(Stack),
 yeccpars2_96(96, ';', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'<\''(Stack),
 yeccpars2_96(96, '<', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'<-\''(Stack),
 yeccpars2_96(96, '<-', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'<<\''(Stack),
 yeccpars2_96(96, '<<', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'<=\''(Stack),
 yeccpars2_96(96, '<=', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'=\''(Stack),
 yeccpars2_96(96, '=', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'=/=\''(Stack),
 yeccpars2_96(96, '=/=', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'=:=\''(Stack),
 yeccpars2_96(96, '=:=', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'=<\''(Stack),
 yeccpars2_96(96, '=<', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'==\''(Stack),
 yeccpars2_96(96, '==', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'=>\''(Stack),
 yeccpars2_96(96, '=>', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'>\''(Stack),
 yeccpars2_96(96, '>', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'>=\''(Stack),
 yeccpars2_96(96, '>=', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'>>\''(Stack),
 yeccpars2_96(96, '>>', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'?\''(Stack),
 yeccpars2_96(96, '?', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'[\''(Stack),
 yeccpars2_96(96, '[', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'after\''(Stack),
 yeccpars2_96(96, 'after', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'and\''(Stack),
 yeccpars2_96(96, 'and', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'andalso\''(Stack),
 yeccpars2_96(96, 'andalso', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_atom(Stack),
 yeccpars2_96(96, atom, [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'band\''(Stack),
 yeccpars2_96(96, 'band', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'begin\''(Stack),
 yeccpars2_96(96, 'begin', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'bnot\''(Stack),
 yeccpars2_96(96, 'bnot', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'bor\''(Stack),
 yeccpars2_96(96, 'bor', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'bsl\''(Stack),
 yeccpars2_96(96, 'bsl', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'bsr\''(Stack),
 yeccpars2_96(96, 'bsr', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'bxor\''(Stack),
 yeccpars2_96(96, 'bxor', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, callback, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_callback(Stack),
 yeccpars2_96(96, callback, [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'case\''(Stack),
 yeccpars2_96(96, 'case', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'catch\''(Stack),
 yeccpars2_96(96, 'catch', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_char(Stack),
 yeccpars2_96(96, char, [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_comment(Stack),
 yeccpars2_96(96, comment, [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'div\''(Stack),
 yeccpars2_96(96, 'div', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'end\''(Stack),
 yeccpars2_96(96, 'end', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_float(Stack),
 yeccpars2_96(96, float, [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'fun\''(Stack),
 yeccpars2_96(96, 'fun', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'if\''(Stack),
 yeccpars2_96(96, 'if', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_integer(Stack),
 yeccpars2_96(96, integer, [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'not\''(Stack),
 yeccpars2_96(96, 'not', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'of\''(Stack),
 yeccpars2_96(96, 'of', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'or\''(Stack),
 yeccpars2_96(96, 'or', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'orelse\''(Stack),
 yeccpars2_96(96, 'orelse', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'receive\''(Stack),
 yeccpars2_96(96, 'receive', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'rem\''(Stack),
 yeccpars2_96(96, 'rem', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_spec(Stack),
 yeccpars2_96(96, spec, [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_string(Stack),
 yeccpars2_96(96, string, [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'try\''(Stack),
 yeccpars2_96(96, 'try', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_var(Stack),
 yeccpars2_96(96, var, [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'when\''(Stack),
 yeccpars2_96(96, 'when', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'xor\''(Stack),
 yeccpars2_96(96, 'xor', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'{\''(Stack),
 yeccpars2_96(96, '{', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'|\''(Stack),
 yeccpars2_96(96, '|', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_105_\'||\''(Stack),
 yeccpars2_96(96, '||', [105 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_105_(Stack),
 yeccpars2_113(113, Cat, [105 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_106(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'!\''(Stack),
 yeccpars2_96(96, '!', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'#\''(Stack),
 yeccpars2_96(96, '#', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'(\''(Stack),
 yeccpars2_96(96, '(', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'*\''(Stack),
 yeccpars2_96(96, '*', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'+\''(Stack),
 yeccpars2_96(96, '+', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'++\''(Stack),
 yeccpars2_96(96, '++', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'-\''(Stack),
 yeccpars2_96(96, '-', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'--\''(Stack),
 yeccpars2_96(96, '--', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'->\''(Stack),
 yeccpars2_96(96, '->', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'.\''(Stack),
 yeccpars2_96(96, '.', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'..\''(Stack),
 yeccpars2_96(96, '..', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'...\''(Stack),
 yeccpars2_96(96, '...', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'/\''(Stack),
 yeccpars2_96(96, '/', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'/=\''(Stack),
 yeccpars2_96(96, '/=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\':\''(Stack),
 yeccpars2_96(96, ':', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'::\''(Stack),
 yeccpars2_96(96, '::', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\':=\''(Stack),
 yeccpars2_96(96, ':=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\';\''(Stack),
 yeccpars2_96(96, ';', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'<\''(Stack),
 yeccpars2_96(96, '<', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'<-\''(Stack),
 yeccpars2_96(96, '<-', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'<<\''(Stack),
 yeccpars2_96(96, '<<', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'<=\''(Stack),
 yeccpars2_96(96, '<=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'=\''(Stack),
 yeccpars2_96(96, '=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'=/=\''(Stack),
 yeccpars2_96(96, '=/=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'=:=\''(Stack),
 yeccpars2_96(96, '=:=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'=<\''(Stack),
 yeccpars2_96(96, '=<', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'==\''(Stack),
 yeccpars2_96(96, '==', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'=>\''(Stack),
 yeccpars2_96(96, '=>', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'>\''(Stack),
 yeccpars2_96(96, '>', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'>=\''(Stack),
 yeccpars2_96(96, '>=', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'>>\''(Stack),
 yeccpars2_96(96, '>>', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'?\''(Stack),
 yeccpars2_96(96, '?', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'[\''(Stack),
 yeccpars2_96(96, '[', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'after\''(Stack),
 yeccpars2_96(96, 'after', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'and\''(Stack),
 yeccpars2_96(96, 'and', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'andalso\''(Stack),
 yeccpars2_96(96, 'andalso', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_atom(Stack),
 yeccpars2_96(96, atom, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'band\''(Stack),
 yeccpars2_96(96, 'band', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'begin\''(Stack),
 yeccpars2_96(96, 'begin', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'bnot\''(Stack),
 yeccpars2_96(96, 'bnot', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'bor\''(Stack),
 yeccpars2_96(96, 'bor', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'bsl\''(Stack),
 yeccpars2_96(96, 'bsl', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'bsr\''(Stack),
 yeccpars2_96(96, 'bsr', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'bxor\''(Stack),
 yeccpars2_96(96, 'bxor', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, callback, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_callback(Stack),
 yeccpars2_96(96, callback, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'case\''(Stack),
 yeccpars2_96(96, 'case', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'catch\''(Stack),
 yeccpars2_96(96, 'catch', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, char, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_char(Stack),
 yeccpars2_96(96, char, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_comment(Stack),
 yeccpars2_96(96, comment, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'div\''(Stack),
 yeccpars2_96(96, 'div', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'end\''(Stack),
 yeccpars2_96(96, 'end', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, float, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_float(Stack),
 yeccpars2_96(96, float, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'fun\''(Stack),
 yeccpars2_96(96, 'fun', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'if\''(Stack),
 yeccpars2_96(96, 'if', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_integer(Stack),
 yeccpars2_96(96, integer, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'not\''(Stack),
 yeccpars2_96(96, 'not', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'of\''(Stack),
 yeccpars2_96(96, 'of', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'or\''(Stack),
 yeccpars2_96(96, 'or', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'orelse\''(Stack),
 yeccpars2_96(96, 'orelse', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'receive\''(Stack),
 yeccpars2_96(96, 'receive', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'rem\''(Stack),
 yeccpars2_96(96, 'rem', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_spec(Stack),
 yeccpars2_96(96, spec, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, string, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_string(Stack),
 yeccpars2_96(96, string, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'try\''(Stack),
 yeccpars2_96(96, 'try', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, var, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_var(Stack),
 yeccpars2_96(96, var, [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'when\''(Stack),
 yeccpars2_96(96, 'when', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'xor\''(Stack),
 yeccpars2_96(96, 'xor', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'{\''(Stack),
 yeccpars2_96(96, '{', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'|\''(Stack),
 yeccpars2_96(96, '|', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_106_\'||\''(Stack),
 yeccpars2_96(96, '||', [106 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_106_(Stack),
 yeccpars2_109(109, Cat, [106 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_107(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionList\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_108(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '..', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '...', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=>', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, callback, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), callback, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), comment, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'NonEmptyExpressionList\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_109/7}).
yeccpars2_109(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_110_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 yeccpars2_96(96, Cat, [111 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_112(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '..', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '...', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':=', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=>', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, callback, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), callback, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), comment, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_112_(Stack),
 'yeccgoto_\'NonEmptyExpressionList\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_113/7}).
yeccpars2_113(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 114, Ss, Stack, T, Ts, Tzr);
yeccpars2_113(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_114_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_115/7}).
yeccpars2_115(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_115(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_116_(Stack),
 'yeccgoto_\'MacroString\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_117/7}).
yeccpars2_117(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_117(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_118_(Stack),
 'yeccgoto_\'NonEmptyExpression\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_119_(Stack),
 yeccpars2_96(96, Cat, [119 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_120(_S, '!', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '!', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '*', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '+', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '++', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '++', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '-', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '--', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '--', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '->', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '->', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '.', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '..', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '...', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '...', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '::', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '::', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, ':=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ':=', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '<-', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<-', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '<<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<<', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '<=', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '=/=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=/=', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '=:=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=:=', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '=<', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=<', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '==', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '==', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '=>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '=>', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>=', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '>>', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '>>', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '?', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '?', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'after', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'after', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'and', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'and', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'andalso', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'andalso', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'band', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'band', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'begin', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'begin', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'bnot', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bnot', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'bor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bor', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'bsl', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsl', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'bsr', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bsr', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'bxor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'bxor', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, callback, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), callback, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'case', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'case', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'catch', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, char, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), char, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, comment, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), comment, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'div', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'div', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'end', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'end', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, float, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), float, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'fun', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'if', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'if', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, integer, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), integer, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'not', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'not', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'of', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'or', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'or', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'orelse', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'orelse', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'receive', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'rem', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'rem', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, spec, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), spec, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, string, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'try', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'try', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, var, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'when', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'when', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, 'xor', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), 'xor', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '|', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Expression\''(hd(Ss), '||', Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_120_(Stack),
 'yeccgoto_\'NonEmptyApplyMacroArgs\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_121/7}).
yeccpars2_121(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_121(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_122/7}).
yeccpars2_122(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_122(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_123/7}).
yeccpars2_123(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_124/7}).
yeccpars2_124(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_125/7}).
yeccpars2_125(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_125(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_126/7}).
yeccpars2_126(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_127: see yeccpars2_47

-dialyzer({nowarn_function, yeccpars2_128/7}).
yeccpars2_128(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_128(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_129/7}).
yeccpars2_129(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_129(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_130_(Stack),
 'yeccgoto_\'Undef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_131/7}).
yeccpars2_131(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_132/7}).
yeccpars2_132(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_133/7}).
yeccpars2_133(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_133(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_134_(Stack),
 'yeccgoto_\'IncludeLib\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_135/7}).
yeccpars2_135(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_135(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_136/7}).
yeccpars2_136(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_136(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_137/7}).
yeccpars2_137(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_137(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_138_(Stack),
 'yeccgoto_\'Include\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_139: see yeccpars2_47

-dialyzer({nowarn_function, yeccpars2_140/7}).
yeccpars2_140(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_141/7}).
yeccpars2_141(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_142_(Stack),
 'yeccgoto_\'IfNDef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_143: see yeccpars2_47

-dialyzer({nowarn_function, yeccpars2_144/7}).
yeccpars2_144(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 145, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_145/7}).
yeccpars2_145(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_146_(Stack),
 'yeccgoto_\'IfDef\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_147: see yeccpars2_47

-dialyzer({nowarn_function, yeccpars2_148/7}).
yeccpars2_148(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_149(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_149_(Stack),
 yeccpars2_160(160, Cat, [149 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_150/7}).
yeccpars2_150(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_151(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_151_(Stack),
 yeccpars2_152(152, Cat, [151 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_152(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_96(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_153_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_154_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_155_(Stack),
 'yeccgoto_\'FormTokens\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_156(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_157_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_158_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_159(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'MacroArgs\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_160/7}).
yeccpars2_160(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_160(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_161(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_161_(Stack),
 'yeccgoto_\'NonEmptyMacroArgs\''(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_162/7}).
yeccpars2_162(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_163_(Stack),
 yeccpars2_164(164, Cat, [163 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_164(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_96(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_165(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'Token\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_166_(Stack),
 'yeccgoto_\'Define\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_167/7}).
yeccpars2_167(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_167(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_168_(Stack),
 'yeccgoto_\'NonEmptyMacroArgs\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_169(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_96(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_170_(Stack),
 'yeccgoto_\'IfBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_171_(Stack),
 yeccpars2_177(177, Cat, [171 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_172(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, else_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_173/7}).
yeccpars2_173(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 176, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_174/7}).
yeccpars2_174(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 175, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_(Stack),
 'yeccgoto_\'EndIf\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_176_(Stack),
 'yeccgoto_\'Else\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_177(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, dot, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_96(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_178_(Stack),
 'yeccgoto_\'IfBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_179(S, define_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, endif_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, ifdef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, ifndef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, include_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, include_lib_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, undef_keyword, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 126, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 'yeccgoto_\'ExpressionToken\''(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_180: see yeccpars2_169

yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_181_(Stack),
 'yeccgoto_\'IfNBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_182_(Stack),
 yeccpars2_177(183, Cat, [182 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_183: see yeccpars2_177

yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_184_(Stack),
 'yeccgoto_\'IfNBlock\''(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ApplyMacroArgs\''/7}).
'yeccgoto_\'ApplyMacroArgs\''(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(97, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Define\''/7}).
'yeccgoto_\'Define\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Define\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Elements\''/7}).
'yeccgoto_\'Elements\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(8, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(180, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(10, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(169, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(171, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Elements\''(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(183, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Else\''/7}).
'yeccgoto_\'Else\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Else\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'EndIf\''/7}).
'yeccgoto_\'EndIf\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'EndIf\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Expression\''/7}).
'yeccgoto_\'Expression\''(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Expression\''(119, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(96, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ExpressionList\''/7}).
'yeccgoto_\'ExpressionList\''(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(117, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionList\''(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(113, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionList\''(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'ExpressionToken\''/7}).
'yeccgoto_\'ExpressionToken\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'ExpressionToken\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'File\''/7}).
'yeccgoto_\'File\''(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'FormTokens\''/7}).
'yeccgoto_\'FormTokens\''(151, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(152, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'FormTokens\''(163, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(164, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'IfBlock\''/7}).
'yeccgoto_\'IfBlock\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfBlock\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'IfDef\''/7}).
'yeccgoto_\'IfDef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfDef\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'IfNBlock\''/7}).
'yeccgoto_\'IfNBlock\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNBlock\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'IfNDef\''/7}).
'yeccgoto_\'IfNDef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IfNDef\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Include\''/7}).
'yeccgoto_\'Include\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Include\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'IncludeLib\''/7}).
'yeccgoto_\'IncludeLib\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'IncludeLib\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Macro\''/7}).
'yeccgoto_\'Macro\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Macro\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'MacroArgs\''/7}).
'yeccgoto_\'MacroArgs\''(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(160, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'MacroName\''/7}).
'yeccgoto_\'MacroName\''(47, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(104, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(90, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(127, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(128, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(139, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(140, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(143, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(144, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroName\''(147, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(148, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'MacroString\''/7}).
'yeccgoto_\'MacroString\''(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroString\''(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'MacroString\''(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'NonEmptyApplyMacroArgs\''/7}).
'yeccgoto_\'NonEmptyApplyMacroArgs\''(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'NonEmptyExpression\''/7}).
'yeccgoto_\'NonEmptyExpression\''(93=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(102=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(105=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpression\''(119=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'NonEmptyExpressionList\''/7}).
'yeccgoto_\'NonEmptyExpressionList\''(102, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpressionList\''(105, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'NonEmptyExpressionList\''(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'NonEmptyMacroArgs\''/7}).
'yeccgoto_\'NonEmptyMacroArgs\''(149, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(159, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Token\''/7}).
'yeccgoto_\'Token\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(152=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(164=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Token\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, 'yeccgoto_\'Undef\''/7}).
'yeccgoto_\'Undef\''(2=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(169=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(177=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(180=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);
'yeccgoto_\'Undef\''(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 60).
yeccpars2_0_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_2_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 58).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ { eof , 0 } ]
  end | __Stack].

-compile({inline,yeccpars2_3_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 63).
yeccpars2_3_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 68).
yeccpars2_4_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 64).
yeccpars2_5_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 67).
yeccpars2_6_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_7_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 66).
yeccpars2_7_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 60).
yeccpars2_8_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_9_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 62).
yeccpars2_9_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_10_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 60).
yeccpars2_10_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_11_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 61).
yeccpars2_11_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 65).
yeccpars2_13_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_67_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 69).
yeccpars2_67_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 57).
yeccpars2_69_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 96).
yeccpars2_90_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_93_\'!\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'!\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'#\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'#\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'(\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'(\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'*\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'*\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'+\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'+\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'++\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'++\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'-\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'-\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'--\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'--\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'->\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'->\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'.\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'.\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'..\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'..\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'...\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'...\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'/\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'/\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'/=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'/=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\':\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\':\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'::\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'::\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\':=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\':=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\';\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\';\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'<-\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'<-\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'<<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'<<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'<=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'<=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'=/=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'=/=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'=:=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'=:=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'=<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'=<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'==\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'==\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'=>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'=>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'>=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'>=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'>>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'>>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'?\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'?\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'[\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'[\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'after\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'after\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'and\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'and\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'andalso\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'andalso\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_atom/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_93_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'band\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'band\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'begin\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'begin\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'bnot\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'bnot\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'bor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'bor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'bsl\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'bsl\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'bsr\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'bsr\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'bxor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'bxor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_callback/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_93_callback(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'case\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'case\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'catch\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'catch\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_char/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_93_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_comment/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_93_comment(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'div\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'div\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'end\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'end\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_float/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_93_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'fun\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'fun\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'if\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'if\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_integer/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_93_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'not\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'not\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'of\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'of\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'or\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'or\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'orelse\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'orelse\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'receive\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'receive\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'rem\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'rem\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_spec/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_93_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_string/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_93_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'try\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'try\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_var/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_93_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'when\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'when\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'xor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'xor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'{\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'{\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'|\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'|\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_93_\'||\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_93_\'||\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_93_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 110).
yeccpars2_93_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_94_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 113).
yeccpars2_94_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_98_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 97).
yeccpars2_98_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 118).
yeccpars2_99_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 117).
yeccpars2_100_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_101_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 116).
yeccpars2_101_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,'yeccpars2_102_\'!\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'!\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'#\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'#\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'(\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'(\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'*\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'*\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'+\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'+\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'++\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'++\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'-\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'-\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'--\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'--\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'->\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'->\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'.\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'.\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'..\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'..\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'...\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'...\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'/\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'/\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'/=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'/=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\':\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\':\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'::\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'::\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\':=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\':=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\';\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\';\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'<-\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'<-\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'<<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'<<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'<=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'<=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'=/=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'=/=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'=:=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'=:=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'=<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'=<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'==\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'==\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'=>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'=>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'>=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'>=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'>>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'>>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'?\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'?\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'[\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'[\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'after\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'after\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'and\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'and\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'andalso\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'andalso\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_atom/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_102_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'band\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'band\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'begin\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'begin\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'bnot\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'bnot\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'bor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'bor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'bsl\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'bsl\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'bsr\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'bsr\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'bxor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'bxor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_callback/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_102_callback(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'case\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'case\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'catch\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'catch\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_char/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_102_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_comment/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_102_comment(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'div\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'div\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'end\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'end\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_float/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_102_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'fun\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'fun\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'if\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'if\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_integer/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_102_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'not\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'not\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'of\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'of\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'or\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'or\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'orelse\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'orelse\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'receive\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'receive\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'rem\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'rem\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_spec/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_102_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_string/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_102_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'try\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'try\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_var/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_102_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'when\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'when\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'xor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'xor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'{\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'{\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'|\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'|\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_102_\'||\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_102_\'||\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_102_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 126).
yeccpars2_102_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'!\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'!\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'#\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'#\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'(\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'(\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'*\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'*\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'+\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'+\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'++\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'++\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'-\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'-\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'--\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'--\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'->\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'->\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'.\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'.\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'..\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'..\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'...\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'...\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'/\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'/\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'/=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'/=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\':\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\':\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'::\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'::\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\':=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\':=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\';\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\';\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'<-\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'<-\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'<<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'<<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'<=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'<=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'=/=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'=/=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'=:=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'=:=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'=<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'=<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'==\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'==\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'=>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'=>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'>=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'>=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'>>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'>>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'?\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'?\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'[\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'[\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'after\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'after\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'and\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'and\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'andalso\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'andalso\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_atom/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_105_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'band\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'band\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'begin\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'begin\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'bnot\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'bnot\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'bor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'bor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'bsl\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'bsl\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'bsr\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'bsr\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'bxor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'bxor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_callback/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_105_callback(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'case\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'case\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'catch\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'catch\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_char/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_105_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_comment/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_105_comment(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'div\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'div\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'end\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'end\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_float/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_105_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'fun\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'fun\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'if\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'if\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_integer/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_105_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'not\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'not\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'of\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'of\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'or\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'or\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'orelse\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'orelse\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'receive\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'receive\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'rem\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'rem\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_spec/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_105_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_string/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_105_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'try\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'try\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_var/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_105_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'when\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'when\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'xor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'xor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'{\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'{\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'|\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'|\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_105_\'||\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_105_\'||\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_105_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 126).
yeccpars2_105_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'!\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'!\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'#\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'#\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'(\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'(\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'*\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'*\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'+\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'+\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'++\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'++\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'-\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'-\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'--\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'--\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'->\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'->\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'.\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'.\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'..\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'..\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'...\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'...\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'/\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'/\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'/=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'/=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\':\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\':\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'::\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'::\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\':=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\':=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\';\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\';\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'<-\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'<-\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'<<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'<<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'<=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'<=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'=/=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'=/=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'=:=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'=:=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'=<\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'=<\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'==\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'==\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'=>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'=>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'>=\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'>=\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'>>\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'>>\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'?\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'?\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'[\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'[\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'after\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'after\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'and\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'and\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'andalso\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'andalso\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_atom/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_106_atom(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'band\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'band\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'begin\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'begin\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'bnot\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'bnot\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'bor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'bor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'bsl\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'bsl\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'bsr\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'bsr\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'bxor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'bxor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_callback/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_106_callback(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'case\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'case\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'catch\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'catch\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_char/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_106_char(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_comment/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_106_comment(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'div\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'div\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'end\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'end\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_float/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_106_float(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'fun\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'fun\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'if\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'if\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_integer/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_106_integer(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'not\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'not\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'of\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'of\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'or\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'or\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'orelse\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'orelse\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'receive\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'receive\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'rem\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'rem\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_spec/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_106_spec(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_string/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_106_string(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'try\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'try\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_var/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_106_var(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'when\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'when\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'xor\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'xor\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'{\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'{\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'|\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'|\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_106_\'||\''/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
'yeccpars2_106_\'||\''(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_106_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 126).
yeccpars2_106_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_110_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 120).
yeccpars2_110_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_111_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_111_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_112_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 129).
yeccpars2_112_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 121).
yeccpars2_114_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 99).
yeccpars2_116_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_string , __3 }
  end | __Stack].

-compile({inline,yeccpars2_118_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 119).
yeccpars2_118_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ] ++ __3 ++ [ __4 ]
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 123).
yeccpars2_119_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_120_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 114).
yeccpars2_120_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __3 ]
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 94).
yeccpars2_130_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_undef , __4 }
  end | __Stack].

-compile({inline,yeccpars2_134_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 81).
yeccpars2_134_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_include_lib , __4 }
  end | __Stack].

-compile({inline,yeccpars2_138_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 80).
yeccpars2_138_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_include , __4 }
  end | __Stack].

-compile({inline,yeccpars2_142_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 90).
yeccpars2_142_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __4
  end | __Stack].

-compile({inline,yeccpars2_146_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 89).
yeccpars2_146_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __4
  end | __Stack].

-compile({inline,yeccpars2_149_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 104).
yeccpars2_149_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_151_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 71).
yeccpars2_151_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_153_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 72).
yeccpars2_153_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 74).
yeccpars2_154_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_155_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 73).
yeccpars2_155_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_157_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 77).
yeccpars2_157_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 , __6 }
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 76).
yeccpars2_158_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 }
  end | __Stack].

-compile({inline,yeccpars2_161_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 108).
yeccpars2_161_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ [ __1 ] ]
  end | __Stack].

-compile({inline,yeccpars2_163_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 71).
yeccpars2_163_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_166_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 78).
yeccpars2_166_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_define , __4 , __6 , __9 }
  end | __Stack].

-compile({inline,yeccpars2_168_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 107).
yeccpars2_168_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ [ __3 ] ]
  end | __Stack].

-compile({inline,yeccpars2_170_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 84).
yeccpars2_170_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifdef , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_171_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 60).
yeccpars2_171_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_175_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 0).
yeccpars2_175_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,yeccpars2_176_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 0).
yeccpars2_176_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,yeccpars2_178_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 83).
yeccpars2_178_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifdef , __1 , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_181_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 87).
yeccpars2_181_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifndef , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_182_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 60).
yeccpars2_182_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_184_/1}).
-file("/home/pavel/repos/github/bearmug/erlang-pg-migrations/_build/lint/plugins/aleppo/src/aleppo_parser.yrl", 86).
yeccpars2_184_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { macro_ifndef , __1 , __2 , __4 }
  end | __Stack].


