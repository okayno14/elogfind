-module(elogfind).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-define(LOG_LEVEL_LABEL_LIST, [
    "ERROR", "WARN", "INFO", "TRACE", "DEBUG"
]).

-record(args, {line_target :: string(), file :: string()}).

%%====================================================================
%% View
%%====================================================================

%% TODO подумать над логикой парсинга ключей и генерации аргсов. Думаю, что лучше завести разные типы опций
%% TODO переименовать -sep во что-то другое, т.к. семантически не очень красиво
%% TODO добавить хелпу, предупредить, что чтение по stdin будет медленным
%% escript Entry point
main(Argv) ->
    Args = parse_args(Argv, #args{}),
    case Args =/= #args{} of
        true when Args#args.file =/= undefined ->
            case file:open(Args#args.file, [read]) of
                {ok, IoDevice} ->
                    read_lines(IoDevice, Args#args.line_target);

                {error, Reason} ->
                    io:format(standard_error, "Failed to open file ~p by Reason:~p", [Args#args.file, Reason]),
                    halt(1)
            end;

        %% значит, что-то прочитали
        true ->
            read_lines(standard_io, Args#args.line_target);

        _false ->
            ok
    end,
    erlang:halt(0).

parse_args([], Args) ->
    Args;

parse_args(Argv, Args) ->
    {Argv2, Args2} = parse_key(Argv, Args),
    parse_args(Argv2, Args2).

parse_key(["-sep" | T], Args) ->
    case T of
        [LineTarget | T2] ->
            {T2, Args#args{line_target = LineTarget}};

        _ ->
            {T, Args}
    end;

parse_key(["-f" | T], Args) ->
    case T of
        [File | T2] ->
            {T2, Args#args{file = File}};

        _ ->
            {T, Args}
    end;

parse_key([_H | T], Args) ->
    {T, Args}.

%%====================================================================
%% fsm_stdout
%%====================================================================

%%--------------------------------------------------------------------
-spec read_lines(Device :: io:device(), LineTarget :: string()) ->
    [] | string().
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

clean_msg_state(MsgState) ->
    MsgState#msg_state{print = noprint, last = nolast, msg_acc = []}.

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
%% @doc
-spec fsm_begin(Line :: line_input(), LineTarget :: string()) ->
    fsm_res().
%%--------------------------------------------------------------------
fsm_begin(Line, LineTarget) ->
    fsm({input, Line}, msg_state(LineTarget), []).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec fsm_input(Line :: line_input(), MsgState :: msg_state()) ->
    fsm_res().
%%--------------------------------------------------------------------
fsm_input(Line, MsgState) ->
    fsm({input, Line}, MsgState, []).
%%--------------------------------------------------------------------

-type msg_state() :: #msg_state{}.
-type out() :: {print, MsgAcc :: list(string())} | noprint.
-type line_input() :: string() | eof.
-type fsm_res() ::  {[out()], msg_state()}.

%%--------------------------------------------------------------------
%% TODO Сделать для каждого входа отдельную функцию
%% @doc Log fsm
%%     pre: LineTarget - valid
%% @end
-spec fsm(
    Cmd ::
        {input, Line :: line_input()} |
        {check, Line :: line_input()} |
        {out, Line :: line_input()},
    MsgState :: msg_state(),
    ListOut :: [out()]
) ->
    fsm_res().
%%--------------------------------------------------------------------
%% Добавить Line в MsgAcc, сменить или оставить nolast
%% init

fsm(Input = {input, eof}, MsgState = #msg_state{last = nolast}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    fsm({out, eof}, MsgState#msg_state{last = last}, ListOut);

fsm(Input = {input, Line}, MsgState = #msg_state{print = noprint, last = nolast, msg_acc = []}, ListOut = []) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case log_begins(MsgState#msg_state.log_begins_re, Line) of
        true ->
            fsm({check, Line}, MsgState#msg_state{msg_acc = [Line]}, ListOut);

        false ->
            fsm({out, Line}, MsgState#msg_state{last = last, msg_acc = [Line]}, ListOut)
    end;

fsm(Input = {input, Line}, MsgState = #msg_state{print = noprint, last = nolast}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case log_begins(MsgState#msg_state.log_begins_re, Line) of
        true ->
            %% отбрасываем аккумулятор старого сообщения (потому что noprint), начинаем копить новое
            MsgStateNew = clean_msg_state(MsgState),
            fsm({check, Line}, MsgStateNew#msg_state{msg_acc = [Line]}, ListOut);

        false ->
            fsm({check, Line}, MsgState#msg_state{msg_acc = [Line | MsgState#msg_state.msg_acc]}, ListOut)
    end;

fsm(Input = {input, Line}, MsgState = #msg_state{print = print, last = nolast}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case log_begins(MsgState#msg_state.log_begins_re, Line) of
        true ->
            fsm({out, Line}, MsgState#msg_state{last = last}, ListOut);

        false ->
            fsm({out, Line}, MsgState#msg_state{msg_acc = [Line | MsgState#msg_state.msg_acc]}, ListOut)
    end;
%%--------------------------------------------------------------------

%% Проверка полученной строки. Помечает сообщение print, в случае успеха
%% require:
%%   Line содержится в Input и хранится в MsgState#msg_state.msg_acc
fsm(Input = {check, Line}, MsgState = #msg_state{print = noprint, last = nolast}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case match_target(Line, MsgState#msg_state.line_target) of
        true ->
            fsm({out, Line}, MsgState#msg_state{print = print}, ListOut);

        false ->
            fsm({out, Line}, MsgState, ListOut)
    end;

fsm(Input = {check, Line}, MsgState = #msg_state{print = noprint, last = last}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    case match_target(Line, MsgState#msg_state.line_target) of
        true ->
            fsm({out, Line}, MsgState#msg_state{print = print}, ListOut);

        false ->
            fsm({out, Line}, MsgState, ListOut)
    end;
%%--------------------------------------------------------------------

%% Line не относится к текущему сообщению, делаем переход:
%% Переложить MsgAcc в ListOut, перейти в Input с Line
fsm(Input = {out, Line}, MsgState = #msg_state{print = print, last = last}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    ListOut2 = [{print, lists:reverse(MsgState#msg_state.msg_acc)} | ListOut],
    case Line of
        eof ->
            {lists:reverse(ListOut2), clean_msg_state(MsgState)};
        _ ->
            fsm({input, Line}, clean_msg_state(MsgState), ListOut2)
    end;

%% Возвращаем noprint, т.к. пропарсили всё сообщение и не нашли LineTarget
fsm(Input = {out, _}, MsgState = #msg_state{print = noprint, last = last}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~ts", [Input, print_msg_state(MsgState)]),
    {lists:reverse([noprint | ListOut]), clean_msg_state(MsgState)};

%% Сообщение ещё полностью не обработано. Просто выходим.
fsm(Input = {out, _}, MsgState = #msg_state{last = nolast}, ListOut) ->
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

