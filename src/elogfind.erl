-module(elogfind).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-define(LOG_LEVEL_LABEL_LIST, [
    "ERROR", "WARN", "INFO", "TRACE", "DEBUG"
]).

-record(args, {line_target :: string()}).

%%====================================================================
%% View
%%====================================================================

%% TODO Удалить ListOut
%% TODO 1 file name, 1 LineTarget
%% escript Entry point
main(Argv) ->
    Args = parse_args(Argv, #args{}),
    case Args =/= #args{} of
        %% значит, что-то прочитали
        true ->
            read_stdin_lines(Args#args.line_target);

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

parse_key([_H | T], Args) ->
    {T, Args}.

%%====================================================================
%% fsm_stdout
%%====================================================================

%%--------------------------------------------------------------------
-spec read_stdin_lines(LineTarget :: string()) ->
    [] | string().
%%--------------------------------------------------------------------
read_stdin_lines(LineTarget) ->
	read_lines(standard_io, LineTarget).
%%--------------------------------------------------------------------

%% TODO read_lines(file:open/2)

%%--------------------------------------------------------------------
-spec read_lines(Device :: io:device(), LineTarget :: string()) ->
    [] | string().
%%--------------------------------------------------------------------
read_lines(Device, LineTarget) ->
    {_noprint, FSM} = fsm_begin("", LineTarget),
	read_lines(Device, LineTarget, FSM).

read_lines(Device, LineTarget, FSM) ->
     case io:get_line(Device, "") of
		eof ->
			ok;

		String ->
            {Out, FSM2} = fsm_input(String, LineTarget, FSM),
            case Out of
                [{print, Msg}] ->
                    io:format("~ts", [Msg]);

                _noprint ->
                    ok
            end,
			read_lines(Device, LineTarget, FSM2)
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
read_lines_list_tester(LineList, LineTarget) ->
    Acc = [],
    {_noprint, FSM} = fsm_begin("", LineTarget),
    read_lines_list_tester_(LineList, LineTarget, FSM, Acc).

read_lines_list_tester_([], _LineTarget, FSM, Acc) ->
    F = fun
    (Arg = [H | _]) when is_list(H) ->
        Arg;

    (Arg) ->
        [Arg]

    end,
    {lists:flatmap(F, lists:reverse(Acc)), FSM};

read_lines_list_tester_([H | T], LineTarget, FSM, Acc) ->
    {Out, FSM2} = fsm_input(H, LineTarget, FSM),

    case Out of
        [{print, [Msg]}] ->
            read_lines_list_tester_(T, LineTarget, FSM2, [Msg | Acc]);

        [{print, Msg}] ->
            read_lines_list_tester_(T, LineTarget, FSM2, [Msg | Acc]);

        _noprint ->
            read_lines_list_tester_(T, LineTarget, FSM2, Acc)
    end.
%%--------------------------------------------------------------------

%%====================================================================
%% fsm_core
%%====================================================================

%%--------------------------------------------------------------------
%% @doc
-spec fsm_begin(Line :: line_input(), LineTarget :: string()) ->
    fsm_res().
%%--------------------------------------------------------------------
fsm_begin(Line, LineTarget) ->
    fsm({input, Line}, LineTarget, {noprint, nolast, []}, []).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% @doc
-spec fsm_input(Line :: line_input(), LineTarget :: string(), MsgState :: msg_state()) ->
    fsm_res().
%%--------------------------------------------------------------------
fsm_input(Line, LineTarget, MsgState) ->
    fsm({input, Line}, LineTarget, MsgState, []).
%%--------------------------------------------------------------------

-type msg_state() :: {print | noprint, last | nolast, MsgAcc :: list(string())}.
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
    LineTarget :: string(),
    MsgState :: msg_state(),
    ListOut :: [out()]
) ->
    fsm_res().
%%--------------------------------------------------------------------
%% Добавить Line в MsgAcc, сменить или оставить nolast
%% init

fsm(Input = {input, eof}, LineTarget, MsgState = {Print, nolast, MsgAcc}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    fsm({out, eof}, LineTarget, {Print, last, MsgAcc}, ListOut);

fsm(Input = {input, Line}, LineTarget, MsgState = {noprint, nolast, []}, ListOut = []) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    case log_begins(Line) of
        true ->
            fsm({check, Line}, LineTarget, {noprint, nolast, [Line]}, ListOut);

        false ->
            fsm({out, Line}, LineTarget, {noprint, last, [Line]}, ListOut)
    end;

fsm(Input = {input, Line}, LineTarget, MsgState = {noprint, nolast, MsgAcc}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    case log_begins(Line) of
        true ->
            %% отбрасываем аккумулятор старого сообщения (потому что noprint), начинаем копить новое
            fsm({check, Line}, LineTarget, {noprint, nolast, [Line]}, ListOut);

        false ->
            fsm({check, Line}, LineTarget, {noprint, nolast, [Line | MsgAcc]}, ListOut)
    end;

fsm(Input = {input, Line}, LineTarget, MsgState = {print, nolast, MsgAcc}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    case log_begins(Line) of
        true ->
            fsm({out, Line}, LineTarget, {print, last, MsgAcc}, ListOut);

        %% TODO переделать на переход в out
        false ->
            {lists:reverse(ListOut), {print, nolast, [Line | MsgAcc]}}
    end;
%%--------------------------------------------------------------------

%% TODO переделать на переход в out
%% Сменить или оставить noprint
fsm(Input = {check, Line}, LineTarget, MsgState = {noprint, nolast, MsgAcc}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    case match_target(Line, LineTarget) of
        true ->
            {lists:reverse(ListOut), {print, nolast, MsgAcc}};

        false ->
            {lists:reverse(ListOut), MsgState}
    end;

fsm(Input = {check, Line}, LineTarget, MsgState = {noprint, last, MsgAcc}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    case match_target(Line, LineTarget) of
        true ->
            fsm({out, Line}, LineTarget, {print, last, MsgAcc}, ListOut);

        false ->
            fsm({out, Line}, LineTarget, MsgState, ListOut)
    end;
%%--------------------------------------------------------------------

%% pre last
fsm(Input = {out, eof}, _LineTarget, MsgState = {noprint, last, _MsgAcc}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    {lists:reverse([noprint | ListOut]), {noprint, nolast, []}};

fsm(Input = {out, eof}, _LineTarget, MsgState = {print, last, MsgAcc}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    ListOut2 = [{print, lists:reverse(MsgAcc)} | ListOut],
    {ListOut2, {noprint, nolast, []}};

%% bad path - дропнуть MsgAcc, дропнуть Line, выйти в receive
fsm(Input = {out, _Line}, _LineTarget, MsgState = {noprint, last, _MsgAcc}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    {lists:reverse([noprint | ListOut]), {noprint, nolast, []}};

%% good path - переложить MsgAcc в ListOut, переложить Line в input
fsm(Input = {out, Line}, LineTarget, MsgState = {print, last, MsgAcc}, ListOut) ->
    ?LOG_DEBUG("Input:~p MsgState:~p", [Input, MsgState]),
    ListOut2 = [{print, lists:reverse(MsgAcc)} | ListOut],
    fsm({input, Line}, LineTarget, {noprint, nolast, []}, ListOut2).
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% TODO переименовать?
%% @doc
-spec log_begins(Line :: string()) ->
    boolean().
%%--------------------------------------------------------------------
log_begins(Line) ->
    Pred =
    fun(LogLevelLabel) ->
        %% TODO положить в стейт фсмки для экономии времени
        {ok, MP} = re:compile(LogLevelLabel, [caseless]),

        case re:run(Line, MP) of
            nomatch ->
                false;

            _ ->
                true
        end
    end,

    case lists:search(Pred, ?LOG_LEVEL_LABEL_LIST) of
        {value, _} ->
            true;

        false ->
            false
    end.
%%--------------------------------------------------------------------

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
    ?assertEqual({noprint, nolast, []}, FSM),
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
    ?assertEqual({noprint, nolast, []}, FSM),
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
    ?assertEqual({noprint, nolast, []}, FSM),
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
    ?assertEqual({noprint, nolast, []}, FSM),
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
    ?assertEqual({noprint, nolast, []}, FSM),
    ok.

