-module(elogfind).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-define(LOG_LEVEL_LABEL_LIST, [
    "ERROR", "WARN", "INFO", "TRACE", "DEBUG"
]).

-record(args_file, {line_target :: string(), file :: string()}).
-record(args_stdin, {line_target :: string()}).

%%====================================================================
%% View
%%====================================================================

%% TODO подумать, надо ли перетаскивать тестовый клиент в другое место?
%% TODO переименовать -sep во что-то другое, т.к. семантически не очень красиво
%% TODO добавить хелпу, предупредить, что чтение по stdin будет медленным
%%--------------------------------------------------------------------
-spec main(Argv :: [string()]) ->
    non_neg_integer().
%%--------------------------------------------------------------------
main(Argv) ->
    Status =
    case parse_argv(Argv) of
        ArgsSTDIN = #args_stdin{} ->
            read_lines(standard_io, ArgsSTDIN#args_stdin.line_target);

        ArgsFile = #args_file{} ->
            read_from_file(ArgsFile);

        {error, not_found} ->
            {error, "Invalid Args"}
    end,

    case Status of
        {error, Reason} ->
            io:format(standard_error, "Failed by reason: ~ts~n", [Reason]),
            1;

        _ok ->
            0
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec read_from_file(ArgsFile :: #args_file{}) ->
    ok | {error, Reason :: string()}.
%%--------------------------------------------------------------------
read_from_file(ArgsFile) ->
    case file:open(ArgsFile#args_file.file, [read]) of
        {ok, IoDevice} ->
            read_lines(IoDevice, ArgsFile#args_file.line_target);

        {error, Reason} ->
            {error, io_lib:format("Failed to open file ~p by Reason:~p", [ArgsFile#args_file.file, Reason])}
    end.
%%--------------------------------------------------------------------

%% TODO тут неплохо использовать Validation-монаду: проверка правильности ключей
%%--------------------------------------------------------------------
%% @doc
-spec parse_argv(Argv :: [string()]) ->
    #args_stdin{} | #args_file{} | {error, not_found}.
%%--------------------------------------------------------------------
parse_argv(Argv) ->
    WithValueFun =
    fun(ArgMapAcc, Key) ->
        case parse_key(Key, Argv) of
            {ok, Value} ->
                ArgMapAcc#{Key => Value};

            {error, key_not_found} ->
                ArgMapAcc;

            {error, value_not_found} ->
                {error, not_found}
        end
    end,

    run_pipe([
        fun(ArgMapAcc) -> WithValueFun(ArgMapAcc, "-f") end,
        fun(ArgMapAcc) -> WithValueFun(ArgMapAcc, "-sep") end,
        fun(ArgMapAcc) ->
            case maps:is_key("-f", ArgMapAcc) of
                true ->
                    args_file(ArgMapAcc);

                _false ->
                    args_stdin(ArgMapAcc)
            end
        end
    ], #{}).
%%--------------------------------------------------------------------

%% TODO вот тут неплохо бы смотрелась Validation-монада: проверка наличия ключей
%%--------------------------------------------------------------------
%% @doc
-spec args_file(ArgMap :: map()) ->
    #args_file{} | {error, not_found}.
%%--------------------------------------------------------------------
args_file(ArgMap) ->
    run_pipe([
        fun(ArgsFileAcc) ->
            case maps:get("-f", ArgMap, not_found) of
                not_found ->
                    {error, not_found};

                File ->
                    ArgsFileAcc#args_file{file = File}
            end
        end,
        fun(ArgsFileAcc) ->
            case maps:get("-sep", ArgMap, not_found) of
                not_found ->
                    {error, not_found};

                LineTarget ->
                    ArgsFileAcc#args_file{line_target = LineTarget}
            end
        end
    ], #args_file{}).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec args_stdin(ArgMap :: map()) ->
    #args_stdin{} | {error, not_found}.
%%--------------------------------------------------------------------
args_stdin(ArgMap) ->
    run_pipe([
        fun(ArgsSTDINAcc) ->
            case maps:get("-sep", ArgMap, not_found) of
                not_found ->
                    {error, not_found};

                LineTarget ->
                    ArgsSTDINAcc#args_stdin{line_target = LineTarget}
            end
        end
    ], #args_stdin{}).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec parse_key(Key :: string(), Argv :: [string()]) ->
    {ok, Value :: string()} | {error, key_not_found | value_not_found}.
%%--------------------------------------------------------------------
parse_key(Key, Argv) ->
    Pred = fun(E) when E =/= Key -> true; (_E) -> false end,
    run_pipe([
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
%% fsm_stdout
%%====================================================================

%%--------------------------------------------------------------------
-spec read_lines(Device :: io:device(), LineTarget :: string()) ->
    ok.
%%--------------------------------------------------------------------
read_lines(Device, LineTarget) ->
    {_noprint, FSM} = fsm_begin("", LineTarget),
	read_lines_(Device, FSM).

read_lines_(Device, FSM) ->
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
			read_lines_(Device, FSM2)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
-spec read_lines_list_tester(LineList :: [string()], LineTarget :: string()) ->
    {Out :: [string()], FSM :: msg_state()}.
%%--------------------------------------------------------------------
read_lines_list_tester(LineList, LineTarget) ->
    Acc = [],
    {_noprint, FSM} = fsm_begin("", LineTarget),
    read_lines_list_tester_(LineList, FSM, Acc).

read_lines_list_tester_([], FSM, Acc) ->
    F = fun
    (Arg = [H | _]) when is_list(H) ->
        Arg;

    (Arg) ->
        [Arg]

    end,
    {lists:flatmap(F, lists:reverse(Acc)), FSM};

read_lines_list_tester_([H | T], FSM, Acc) ->
    {Out, FSM2} = fsm_input(H, FSM),

    case Out of
        [{print, [Msg]}] ->
            read_lines_list_tester_(T, FSM2, [Msg | Acc]);

        [{print, Msg}] ->
            read_lines_list_tester_(T, FSM2, [Msg | Acc]);

        _noprint ->
            read_lines_list_tester_(T, FSM2, Acc)
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

%% Начальное значение аккумулятора для pipe/compose
-type acc0() :: fun(() -> result()) | term().

%% Конечный результат композиции
-type result() :: {_Result, {error, _Reason}} | {error, _Reason} | error | _Result.

%% Результат функций, собираемых в композицию
-type funlist2() :: [fun((_Acc) -> result2())].
-type result2() :: {dive, funlist2()} | {dive, _Acc, funlist2()} | result().
%%--------------------------------------------------------------------
%% @doc
%% <pre>
%% Пропускает значение по конвейеру функций.
%% FunList - список анонимных функций, по которым будет пропущен аккумулятор.
%%           Если одна из функций вернёт {error, _Reason}, то произойдёт остановка конвейера.
%%           Если одна из функций вернёт {dive, FunList}, то
%%           FunList будет положен в начало оставшегося конвейера (безопасно для стека вызовов).
%% AccFun - функция, возвращающая начальное значение; либо уже заранее определённый аккумулятор
%% pre:
%%   Функции из FunList не должны генерировать исключения
%% </pre>
%% @end
-spec run_pipe(FunList :: funlist2(), AccFun :: acc0()) ->
    result().
%%--------------------------------------------------------------------
run_pipe(FunList, AccFun) when is_function(AccFun) ->
    run_pipe_([[fun(_) -> AccFun() end | FunList]], undefined);

run_pipe(FunList, Acc) ->
    run_pipe_([FunList], Acc).

run_pipe_([], Acc) ->
    Acc;

run_pipe_([[] | T], Acc) ->
    run_pipe_(T, Acc);

run_pipe_([H | T], Acc) ->
     [H2 | T2] = H,
     case H2(Acc) of
        ResultErr = {_Result, {error, _Reason}} ->
            ResultErr;

        ResultErr = {error, _Reason} ->
            ResultErr;

        ResultErr = error ->
            ResultErr;

        {dive, L2} ->
            L3 = [L2] ++ [T2 | T],
            run_pipe_(L3, Acc);

        {dive, Acc2, L2} ->
            L3 = [L2] ++ [T2 | T],
            run_pipe_(L3, Acc2);

        Acc2 ->
            L2 = [T2 | T],
            run_pipe_(L2, Acc2)
    end.
%%--------------------------------------------------------------------

%%%===================================================================
%%% Test
%%%===================================================================

base_test_() ->
    [
        {"1 multiline", fun case1/0},
        {"1 multiline, 1 singleline", fun case2/0},
        {"1 singleline", fun case3/0},
        {"1 multiline, 1 multiline", fun case6/0},
        {"1 multiline not match, 1 singleline", fun case4/0},
        {"1 multiline, 1 singleline not match", fun case5/0}
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

    {Out, FSM} = read_lines_list_tester(SampleList, "hello"),
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

    {Out, FSM} = read_lines_list_tester(SampleList, "hello"),
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

    {Out, FSM} = read_lines_list_tester(SampleList, "hello"),
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

    {Out, FSM} = read_lines_list_tester(SampleList, "hello"),
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

    {Out, FSM} = read_lines_list_tester(SampleList, "hello"),
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

    {Out, FSM} = read_lines_list_tester(SampleList, "hello"),
    ?LOG_DEBUG("=======", []),

    ?assertEqual(FinalList, Out),
    ?assertEqual(msg_state("hello"), FSM),
    ok.

