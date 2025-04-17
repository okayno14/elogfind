-module(elogfind).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-define(LOG_LEVEL_LABEL_LIST, [
    "ERROR",
    "WARN",
    "INFO",
    "TRACE",
    "DEBUG",
    "CONSOLE",
    "NOTICE",
    "WARNING",
    "ERR",
    "CRITICAL",
    "ALERT",
    "EMERGENCY"
]).

-record(cmd_file, {line_target :: string(), file :: string()}).
-record(cmd_stdin, {line_target :: string()}).
-record(cmd_help, {}).

-define(F_OPTION, "-f").
-define(STR_OPTION, "-str").
-define(HELP_OPTION, "--help").

%%====================================================================
%% View
%%====================================================================

%%--------------------------------------------------------------------
-spec main(Argv :: [string()]) ->
    non_neg_integer().
%%--------------------------------------------------------------------
main(Argv) ->
    Status =
    case route_cmd(Argv) of
        CMDSTDIN = #cmd_stdin{} ->
            run_cmd_stdio(CMDSTDIN);

        CMDFile = #cmd_file{} ->
            run_cmd_file(CMDFile);

        #cmd_help{} ->
            print_help();

        Error ->
            Error
    end,

    case Status of
        {error, Reason} ->
            io:format(standard_error, "Failed by reason:~n~ts", [Reason]),
            1;

        _ok ->
            0
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec run_cmd_stdio(CMDSTDIN :: #cmd_stdin{}) ->
    ok | {error, Reason :: string()}.
%%--------------------------------------------------------------------
run_cmd_stdio(CMDSTDIN) ->
    io:setopts(standard_io, [binary]),
    run_fsm_io(standard_io, CMDSTDIN#cmd_stdin.line_target).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec run_cmd_file(CMDFile :: #cmd_file{}) ->
    ok | {error, Reason :: string()}.
%%--------------------------------------------------------------------
run_cmd_file(CMDFile) ->
    case run_fsm_file_stdout(CMDFile#cmd_file.file, CMDFile#cmd_file.line_target) of
        ok ->
            ok;

        {error, {file, Reason}} ->
            {error, io_lib:format("Failed to open file ~ts by Reason: ~p~n", [CMDFile#cmd_file.file, Reason])}
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec print_help() ->
    ok.
%%--------------------------------------------------------------------
print_help() ->
    Str =
    [
        "Filter log-messages by string match.\n",
        "By default reads data from STDIN. This method NOT RECOMMENDED for big logs.\n",
        "Args:\n",
        io_lib:format("    ~ts <String>: match <String> for log-message~n", [?STR_OPTION]),
        io_lib:format("    ~ts <File>: read data from <File>~n", [?F_OPTION]),
        io_lib:format("    ~ts: print this message~n", [?HELP_OPTION])
    ],

    io:format("~ts", [Str]).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Набор строк от юзера из шелла - Argv
%% Мапа с распаршенным вводом - Options
%% Распаршенные рекорды, на основе которых вьюха вызывает нужную функцию - Command
%% </pre>
%% @end
-spec route_cmd(Argv :: [string()]) ->
    #cmd_stdin{} | #cmd_file{} | #cmd_help{} | {error, [Reason :: string()]}.
%%--------------------------------------------------------------------
route_cmd(Argv) ->
    case route_cmd2(Argv) of
        ErrorStack = [{error, _} | _] ->
            {error, [[print_parse_error(ParseError), "\n"] || {error, ParseError} <- ErrorStack]};

        CMD ->
            CMD
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Набор строк от юзера из шелла - Argv
%% Мапа с распаршенным вводом - Options
%% Распаршенные рекорды, на основе которых вьюха вызывает нужную функцию - Command
%% </pre>
%% @end
-spec route_cmd2(Argv :: [string()]) ->
    #cmd_stdin{} | #cmd_file{} | #cmd_help{} | [{error, Reason :: parse_error()}].
%%--------------------------------------------------------------------
route_cmd2(Argv) ->
    Pipe =
    compose:pipe([
        fun
        ({ok, Options}) ->
            either:right(Options);

        ({error, ErrorStack}) ->
            either:left(ErrorStack)

        end,

        fun(Either) ->
            either:map(Either,
                fun
                    (#{?HELP_OPTION := _} = _Options) ->
                        #cmd_help{};

                    (#{?F_OPTION := _} = Options) ->
                        cmd_file(Options);

                    (Options) ->
                        cmd_stdin(Options)
                end
            )
        end
    ]),

    either:extract(Pipe(options(Argv))).
%%--------------------------------------------------------------------

%%%===================================================================
%%% View.command_builder
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec cmd_file(Options :: map()) ->
    #cmd_file{} | [{error, Reason :: parse_error()}].
%%--------------------------------------------------------------------
cmd_file(Options) ->
    CheckOptionFunCurried = curry:curry_right(fun check_option/2),

    Validation =
    compose:run_pipe([
        fun(Validation) -> validation:flatmap(Validation, CheckOptionFunCurried(?F_OPTION)) end,
        fun(Validation) -> validation:flatmap(Validation, CheckOptionFunCurried(?STR_OPTION)) end
    ], validation:validation(Options)),

    case validation:extract_error_stack(Validation) of
        [] ->
            #cmd_file{file = maps:get(?F_OPTION, Options), line_target = maps:get(?STR_OPTION, Options)};

        ErrorStack ->
            ErrorStack
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec cmd_stdin(Options :: map()) ->
    #cmd_stdin{} | [{error, Reason :: parse_error()}].
%%--------------------------------------------------------------------
cmd_stdin(Options) ->
    CheckOptionFunCurried = curry:curry_right(fun check_option/2),

    Validation =
    compose:run_pipe([
        fun(Validation) -> validation:flatmap(Validation, CheckOptionFunCurried(?STR_OPTION)) end
    ], validation:validation(Options)),

    case validation:extract_error_stack(Validation) of
        [] ->
            #cmd_stdin{line_target = maps:get(?STR_OPTION, Options)};

        ErrorStack ->
            ErrorStack
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% View.argv_parse.options_builder
%%%===================================================================

%%--------------------------------------------------------------------
-spec options(Argv :: [string()]) ->
    {ok, map()} | {error, [{error, Reason :: parse_error()}]}.
%%--------------------------------------------------------------------
options(Argv) ->
    WithValueFun =
    fun(OptionsAcc, Key) ->
        case parse_key(Key, Argv) of
            {ok, Value} ->
                validation:validation(OptionsAcc#{Key => Value});

            {error, key_not_found} ->
                validation:validation(OptionsAcc);

            {error, value_not_found} ->
                validation:validation_error([{error, parse_error(Key, value_not_found)}])
        end
    end,

    NoValueFun =
    fun(OptionsAcc, Key) ->
        case parse_key(Key, Argv) of
            {ok, Value} ->
                validation:validation(OptionsAcc#{Key => Value});

            {error, key_not_found} ->
                validation:validation(OptionsAcc);

            {error, value_not_found} ->
                validation:validation(OptionsAcc#{Key => []})
        end
    end,

    WithValueFunCurried = curry:curry_right(WithValueFun),
    NoValueFunCurried = curry:curry_right(NoValueFun),

    ValidationPipe =
    compose:pipe([
        fun(Validation) -> validation:flatmap(Validation, WithValueFunCurried(?F_OPTION)) end,
        fun(Validation) -> validation:flatmap(Validation, WithValueFunCurried(?STR_OPTION)) end,
        fun(Validation) -> validation:flatmap(Validation, NoValueFunCurried(?HELP_OPTION)) end
    ]),

    Validation = ValidationPipe(validation:validation(#{})),
    case validation:extract_error_stack(Validation) of
        [] ->
            {ok, validation:extract(Validation)};

        ErrorStack ->
            {error, ErrorStack}
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% View.argv_parse.options
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec check_option(Options :: map(), Option :: string()) ->
    validation:validation().
%%--------------------------------------------------------------------
check_option(Options, Option) ->
    case maps:get(Option, Options, not_found) of
        not_found ->
            validation:validation_error([{error, parse_error(Option, option_not_present)}]);

        _Value ->
            validation:validation(Options)
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% View.argv_parse.options.parse_error
%%%===================================================================

-type parse_error() :: #{argument := Arg :: string(), reason := value_not_found | option_not_present}.

%%--------------------------------------------------------------------
%% @doc
-spec parse_error(Arg :: string(), Reason :: atom()) ->
    parse_error().
%%--------------------------------------------------------------------
parse_error(Arg, Reason) ->
    #{argument => Arg, reason => Reason}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec print_parse_error(ParseError :: parse_error()) ->
    string().
%%--------------------------------------------------------------------
print_parse_error(ParseError) ->
    #{argument := Arg, reason := Reason} = ParseError,
    case Reason of
        value_not_found ->
            io_lib:format("Value for ~ts MUST be specified", [Arg]);

        option_not_present ->
            io_lib:format("~ts MUST be present", [Arg])
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% View.argv_parse.options.parse_key
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
-spec parse_key(Key :: string(), Argv :: [string()]) ->
    {ok, Value :: string()} | {error, key_not_found | value_not_found}.
%%--------------------------------------------------------------------
parse_key(Key, Argv) ->
    Pred = fun(E) when E =/= Key -> true; (_E) -> false end,
    compose:run_pipe([
        fun(ArgvAcc) ->
            case lists:dropwhile(Pred, ArgvAcc) of
                [] ->
                    {error, key_not_found};

                ArgvAcc2 ->
                    ArgvAcc2
            end
        end,
        fun(ArgvAcc) ->
            case nth(2, ArgvAcc) of
                %% Следующий элемент - ключ, надо вернуть ошибку
                [$- | _] ->
                    {error, value_not_found};

                [] ->
                    {error, value_not_found};

                Elem ->
                    {ok, Elem}
            end

        end
    ], Argv).
%%--------------------------------------------------------------------

%%====================================================================
%% Controller.fsm_runners
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
-spec run_fsm_file_stdout(File :: string(), LineTarget :: string()) ->
    ok | {error, {file, _Reason}}.
%%--------------------------------------------------------------------
run_fsm_file_stdout(File, LineTarget) ->
    case file:open(File, [read, binary]) of
        {ok, IoDevice} ->
            run_fsm_io(IoDevice, LineTarget);

        {error, Reason} ->
            {error, {file, Reason}}
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec run_fsm_io(Device :: io:device(), LineTarget :: string()) ->
    ok.
%%--------------------------------------------------------------------
run_fsm_io(Device, LineTarget) ->
    {_noprint, FSM} = fsm_begin("", LineTarget),
	run_fsm_io_(Device, FSM).

run_fsm_io_(Device, FSM) ->
     case io:get_line(Device, "") of
		eof ->
			ok;

		String ->
            {Out, FSM2} = fsm_input(String, FSM),
            case Out of
                [{print, Msg}] ->
                    io:format("~ts", [Msg]);

                _noprint ->
                    ok
            end,
			run_fsm_io_(Device, FSM2)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec run_fsm_string_list(LineList :: [string()], LineTarget :: string()) ->
    {Out :: [string()], FSM :: msg_state()}.
%%--------------------------------------------------------------------
run_fsm_string_list(LineList, LineTarget) ->
    Acc = [],
    {_noprint, FSM} = fsm_begin("", LineTarget),
    run_fsm_string_list_(LineList, FSM, Acc).

run_fsm_string_list_([], FSM, Acc) ->
    F = fun
    (Arg = [H | _]) when is_list(H) ->
        Arg;

    (Arg) ->
        [Arg]

    end,
    {lists:flatmap(F, lists:reverse(Acc)), FSM};

run_fsm_string_list_([H | T], FSM, Acc) ->
    {Out, FSM2} = fsm_input(H, FSM),

    case Out of
        [{print, [Msg]}] ->
            run_fsm_string_list_(T, FSM2, [Msg | Acc]);

        [{print, Msg}] ->
            run_fsm_string_list_(T, FSM2, [Msg | Acc]);

        _noprint ->
            run_fsm_string_list_(T, FSM2, Acc)
    end.
%%--------------------------------------------------------------------

%%====================================================================
%% fsm_core
%%====================================================================

-record(msg_state, {
    print :: print | noprint,
    last :: last | nolast,
    line_target :: string(),
    msg_acc :: list(string()),
    log_begins_re :: list()
}).

%%--------------------------------------------------------------------
%% @doc
-spec msg_state(LineTarget :: string()) ->
    msg_state().
%%--------------------------------------------------------------------
msg_state(LineTarget) ->
    MakeReFun = fun(X) -> {ok, MP} = re:compile(X, [caseless]), MP end,
    LogBeginsRe = [MakeReFun(LogLevelLabel) || LogLevelLabel <- ?LOG_LEVEL_LABEL_LIST],
    #msg_state{
        print = noprint,
        last = nolast,
        line_target = LineTarget,
        msg_acc = [],
        log_begins_re = LogBeginsRe
    }.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec clean_msg_state(MsgState :: msg_state()) ->
    MsgState2 :: msg_state().
%%--------------------------------------------------------------------
clean_msg_state(MsgState) ->
    MsgState#msg_state{print = noprint, last = nolast, msg_acc = []}.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec print_msg_state(MsgState :: msg_state()) ->
    string().
%%--------------------------------------------------------------------
print_msg_state(MsgState) ->
    PropList =
    lists:keydelete(log_begins_re, 1,
        lists:zip(
            record_info(fields, msg_state),
            erlang:tl(erlang:tuple_to_list(MsgState))
        )
    ),
    io_lib:format("~0tp", [PropList]).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec fsm_begin(Line :: line_input(), LineTarget :: string()) ->
    fsm_res().
%%--------------------------------------------------------------------
fsm_begin(Line, LineTarget) ->
    fsm_input(Line, msg_state(LineTarget), []).
%%--------------------------------------------------------------------

%% TODO исправить спеку - Line может быть бинарём, перепроверить остальные типы
%%--------------------------------------------------------------------
%% @doc
-spec fsm_input(Line :: line_input(), MsgState :: msg_state()) ->
    fsm_res().
%%--------------------------------------------------------------------
fsm_input(Line, MsgState) ->
    fsm_input(Line, MsgState, []).
%%--------------------------------------------------------------------

-type msg_state() :: #msg_state{}.
-type out() :: {print, MsgAcc :: list(string())} | noprint.
-type line_input() :: string() | eof.
-type fsm_res() ::  {[out()], msg_state()}.

%%--------------------------------------------------------------------
%% @doc Определяет границы полученных сообщений
-spec fsm_input(Input :: line_input(), MsgState :: msg_state(), ListOut :: [out()]) ->
    fsm_res().
%%--------------------------------------------------------------------
fsm_input(Input = eof, MsgState = #msg_state{last = nolast}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    fsm_out(eof, MsgState#msg_state{last = last}, ListOut);

fsm_input(Input = Line, MsgState = #msg_state{print = noprint, last = nolast, msg_acc = []}, ListOut = []) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case log_begins(MsgState#msg_state.log_begins_re, Line) of
        true ->
            fsm_check_line(Line, MsgState#msg_state{msg_acc = [Line]}, ListOut);

        %% При обнулённом стейте получили первую строку без метки границы сообщения. Отбрасываем.
        false ->
            fsm_out(Line, MsgState#msg_state{last = last, msg_acc = [Line]}, ListOut)
    end;

fsm_input(Input = Line, MsgState = #msg_state{print = noprint, last = nolast}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case log_begins(MsgState#msg_state.log_begins_re, Line) of
        true ->
            %% отбрасываем аккумулятор старого сообщения (потому что noprint), начинаем копить новое
            MsgStateNew = clean_msg_state(MsgState),
            fsm_check_line(Line, MsgStateNew#msg_state{msg_acc = [Line]}, ListOut);

        false ->
            fsm_check_line(Line, MsgState#msg_state{msg_acc = [Line | MsgState#msg_state.msg_acc]}, ListOut)
    end;

fsm_input(Input = Line, MsgState = #msg_state{print = print, last = nolast}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case log_begins(MsgState#msg_state.log_begins_re, Line) of
        true ->
            fsm_out(Line, MsgState#msg_state{last = last}, ListOut);

        false ->
            fsm_out(Line, MsgState#msg_state{msg_acc = [Line | MsgState#msg_state.msg_acc]}, ListOut)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc Проверка полученной строки. Помечает сообщение print, в случае успеха
-spec fsm_check_line(Line :: line_input(), MsgState :: msg_state(), ListOut :: [out()]) ->
    fsm_res().
%%--------------------------------------------------------------------
fsm_check_line(Input = Line, MsgState = #msg_state{print = noprint, last = nolast}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case match_target(Line, MsgState#msg_state.line_target) of
        true ->
            fsm_out(Line, MsgState#msg_state{print = print}, ListOut);

        false ->
            fsm_out(Line, MsgState, ListOut)
    end;

fsm_check_line(Input = Line, MsgState = #msg_state{print = noprint, last = last}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case match_target(Line, MsgState#msg_state.line_target) of
        true ->
            fsm_out(Line, MsgState#msg_state{print = print}, ListOut);

        false ->
            fsm_out(Line, MsgState, ListOut)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec fsm_out(Line :: line_input(), MsgState :: msg_state(), ListOut :: [out()]) ->
    fsm_res().
%%--------------------------------------------------------------------
%% Line не относится к текущему сообщению, делаем переход:
%% Переложить MsgAcc в ListOut, перейти в Input с Line
fsm_out(Input = Line, MsgState = #msg_state{print = print, last = last}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    ListOut2 = [{print, lists:reverse(MsgState#msg_state.msg_acc)} | ListOut],
    case Line of
        eof ->
            {lists:reverse(ListOut2), clean_msg_state(MsgState)};
        _ ->
            fsm_input(Line, clean_msg_state(MsgState), ListOut2)
    end;

%% Возвращаем noprint, т.к. пропарсили всё сообщение и не нашли LineTarget
fsm_out(Input, MsgState = #msg_state{print = noprint, last = last}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    {lists:reverse([noprint | ListOut]), clean_msg_state(MsgState)};

%% Сообщение ещё полностью не обработано. Просто выходим.
fsm_out(Input, MsgState = #msg_state{last = nolast}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    {lists:reverse(ListOut), MsgState}.
%%--------------------------------------------------------------------

log_begins(LogBeginsRe, Line) ->
    Pred =
    fun(MP) ->
        case re:run(Line, MP) of
            nomatch ->
                false;

            _ ->
                true
        end
    end,

    case lists:search(Pred, LogBeginsRe) of
        {value, _} ->
            true;

        false ->
            false
    end.

%%--------------------------------------------------------------------
%% @doc
-spec match_target(Line :: string(), LineTarget :: string()) ->
    boolean().
%%--------------------------------------------------------------------
match_target(Line, LineTarget) ->
    case string:find(Line, LineTarget) of
        nomatch ->
            false;

        _ ->
            true
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% utils
%%%===================================================================

nth(_N, []) -> [];
nth(1, [H|_]) -> H;
nth(N, [_|T]) when N > 1 ->
    nth(N - 1, T).

%%%===================================================================
%%% Test
%%%===================================================================

route_cmd_test_() ->
    [
        {"stdin cmd", fun stdin_cmd/0},
        {"file cmd", fun file_cmd/0},
        {"help cmd", fun help_cmd/0},
        {"keys without values", fun no_val_cmd/0},
        {"not enough options", fun not_enough_options/0}
    ].

stdin_cmd() ->
    %% INIT
    Argv = [?STR_OPTION, "some_string"],

    %% ACT
    Value = route_cmd2(Argv),

    %% ASSERT
    ?assertEqual(#cmd_stdin{line_target = "some_string"}, Value).

file_cmd() ->
    %% INIT
    Argv = [?STR_OPTION, "some_string", ?F_OPTION, "some_file"],

    %% ACT
    Value = route_cmd2(Argv),

    %% ASSERT
    ?assertEqual(#cmd_file{line_target = "some_string", file = "some_file"}, Value).

help_cmd() ->
    %% INIT
    Argv = [?HELP_OPTION],

    %% ACT
    Value = route_cmd2(Argv),

    %% ASSERT
    ?assertEqual(#cmd_help{}, Value).

no_val_cmd() ->
    %% INIT
    Argv = [?STR_OPTION, ?F_OPTION],

    %% ACT
    Value = route_cmd2(Argv),

    %% ASSERT
    ?assertEqual(
        [
            {error, parse_error(?F_OPTION, value_not_found)},
            {error, parse_error(?STR_OPTION, value_not_found)}
        ],
        Value
    ).

not_enough_options() ->
    %% INIT
    Argv = [?F_OPTION, "some_file"],

    %% ACT
    Value = route_cmd2(Argv),

    %% ASSERT
    ?assertEqual([{error, parse_error(?STR_OPTION, option_not_present)}], Value).

fsm_test_() ->
    [
        {"1 multiline", fun case1/0},
        {"1 multiline, 1 singleline", fun case2/0},
        {"1 singleline", fun case3/0},
        {"1 multiline, 1 multiline", fun case6/0},
        {"1 multiline not match, 1 singleline", fun case4/0},
        {"1 multiline, 1 singleline not match", fun case5/0},
        {"1 singleline, level small case", fun case7/0}
    ].

case1() ->
    SampleList = [
        "INFO some",
        "hello text",
        eof
    ],

    FinalList = [
        "INFO some",
        "hello text"
    ],

    {Out, FSM} = run_fsm_string_list(SampleList, "hello"),
    ?LOG_DEBUG("=======", []),

    ?assertEqual(FinalList, Out),
    ?assertEqual(msg_state("hello"), FSM),
    ok.

case2() ->
    SampleList = [
        "INFO hello",
        "some text",
        "WARNING hello",
        eof
    ],

    FinalList = [
        "INFO hello",
        "some text",
        "WARNING hello"
    ],

    {Out, FSM} = run_fsm_string_list(SampleList, "hello"),
    ?LOG_DEBUG("=======", []),

    ?assertEqual(FinalList, Out),
    ?assertEqual(msg_state("hello"), FSM),
    ok.

case3() ->
    SampleList = [
        "WARNING hello",
        eof
    ],

    FinalList = [
        "WARNING hello"
    ],

    {Out, FSM} = run_fsm_string_list(SampleList, "hello"),
    ?LOG_DEBUG("=======", []),

    ?assertEqual(FinalList, Out),
    ?assertEqual(msg_state("hello"), FSM),
    ok.

case4() ->
    SampleList = [
        "INFO some",
        "some text",
        "WARNING hello",
        eof
    ],

    FinalList = [
        "WARNING hello"
    ],

    {Out, FSM} = run_fsm_string_list(SampleList, "hello"),
    ?LOG_DEBUG("=======", []),

    ?assertEqual(FinalList, Out),
    ?assertEqual(msg_state("hello"), FSM),
    ok.

case5() ->
    SampleList = [
        "INFO hello",
        "some text",
        "WARNING some",
        eof
    ],

    FinalList = [
        "INFO hello",
        "some text"
    ],

    {Out, FSM} = run_fsm_string_list(SampleList, "hello"),
    ?LOG_DEBUG("=======", []),

    ?assertEqual(FinalList, Out),
    ?assertEqual(msg_state("hello"), FSM),
    ok.

case6() ->
    SampleList = [
        "INFO hello",
        "some text",
        "DEBUG fff",
        "hello",
        "world",
        eof
    ],

    FinalList = [
        "INFO hello",
        "some text",
        "DEBUG fff",
        "hello",
        "world"
    ],

    {Out, FSM} = run_fsm_string_list(SampleList, "hello"),
    ?LOG_DEBUG("=======", []),

    ?assertEqual(FinalList, Out),
    ?assertEqual(msg_state("hello"), FSM),
    ok.

case7() ->
    SampleList = [
        "info hello",
        eof
    ],

    FinalList = [
        "info hello"
    ],

    {Out, FSM} = run_fsm_string_list(SampleList, "hello"),
    ?LOG_DEBUG("=======", []),

    ?assertEqual(FinalList, Out),
    ?assertEqual(msg_state("hello"), FSM),
    ok.
