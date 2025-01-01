-module(elogfind).

-include_lib("kernel/include/logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([main/1]).

-define(LOG_LEVEL_LABEL_LIST, [
    "ERROR", "WARN", "INFO", "TRACE", "DEBUG"
]).

%%====================================================================
%% View
%%====================================================================

%% TODO Удалить ListOut
%% TODO 1 file name, 1 LineTarget
%% escript Entry point
main(_Args) ->
    erlang:halt(0).

%%====================================================================
%% fsm_stdout
%%====================================================================

%%--------------------------------------------------------------------
-spec read_stdin_lines() ->
    [] | string().
%%--------------------------------------------------------------------
read_stdin_lines() ->
	read_lines(standard_io).
%%--------------------------------------------------------------------

%% TODO read_lines(file:open/2)

%%--------------------------------------------------------------------
-spec read_lines(Device :: io:device()) ->
    [] | string().
%%--------------------------------------------------------------------
read_lines(Device) ->
	read_lines(Device, []).

read_lines(Device, L) ->
     case io:get_line(Device, "") of
		eof ->
			lists:reverse(L);

		String ->
			read_lines(Device, [string:trim(String, trailing) | L])
    end.
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
read_lines_list_tester(LineList, LineTarget) ->
    Acc = [],
    {_noprint, FSM} = fsm_begin("", LineTarget),
    read_lines_list_tester_(LineList, LineTarget, FSM, Acc).

read_lines_list_tester_([], _LineTarget, _FSM, Acc) ->
    Acc;

read_lines_list_tester_([H | T], LineTarget, FSM, Acc) ->
    {Out, FSM2} = fsm_input(H, LineTarget, FSM),

    case Out of
        %% TODO пока так
        [{print, [Msg]}] ->
            read_lines_list_tester_(T, LineTarget, FSM2, Acc ++ [Msg]);

        [{print, Msg}] ->
            read_lines_list_tester_(T, LineTarget, FSM2, Acc ++ Msg);

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
%% TODO добавить логи fsm: СтейтCur -> СтейтNext: Вход
%% TODO добавить тесты, написать более обобщённо
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
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
    fsm({out, eof}, LineTarget, {Print, last, MsgAcc}, ListOut);

fsm(Input = {input, Line}, LineTarget, MsgState = {noprint, nolast, []}, ListOut = []) ->
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
    case log_begins(Line) of
        true ->
            fsm({check, Line}, LineTarget, {noprint, nolast, [Line]}, ListOut);

        false ->
            fsm({out, Line}, LineTarget, {noprint, last, [Line]}, ListOut)
    end;

fsm(Input = {input, Line}, LineTarget, MsgState = {noprint, nolast, MsgAcc}, ListOut) ->
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
    case log_begins(Line) of
        true ->
            %% отбрасываем аккумулятор старого сообщения (потому что noprint), начинаем копить новое
            fsm({check, Line}, LineTarget, {noprint, nolast, [Line]}, ListOut);

        false ->
            fsm({check, Line}, LineTarget, {noprint, nolast, [Line | MsgAcc]}, ListOut)
    end;

fsm(Input = {input, Line}, LineTarget, MsgState = {print, nolast, MsgAcc}, ListOut) ->
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
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
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
    case match_target(Line, LineTarget) of
        true ->
            {lists:reverse(ListOut), {print, nolast, MsgAcc}};

        false ->
            {lists:reverse(ListOut), MsgState}
    end;

fsm(Input = {check, Line}, LineTarget, MsgState = {noprint, last, MsgAcc}, ListOut) ->
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
    case match_target(Line, LineTarget) of
        true ->
            fsm({out, Line}, LineTarget, {print, last, MsgAcc}, ListOut);

        false ->
            fsm({out, Line}, LineTarget, MsgState, ListOut)
    end;
%%--------------------------------------------------------------------

%% pre last
fsm(Input = {out, eof}, _LineTarget, MsgState = {noprint, last, _MsgAcc}, ListOut) ->
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
    {lists:reverse([noprint | ListOut]), {noprint, nolast, []}};

fsm(Input = {out, eof}, _LineTarget, MsgState = {print, last, MsgAcc}, ListOut) ->
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
    ListOut2 = [{print, lists:reverse(MsgAcc)} | ListOut],
    {ListOut2, {noprint, nolast, []}};

%% bad path - дропнуть MsgAcc, дропнуть Line, выйти в receive
fsm(Input = {out, _Line}, _LineTarget, MsgState = {noprint, last, _MsgAcc}, ListOut) ->
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
    {lists:reverse([noprint | ListOut]), {noprint, nolast, []}};

%% good path - переложить MsgAcc в ListOut, переложить Line в input
fsm(Input = {out, Line}, LineTarget, MsgState = {print, last, MsgAcc}, ListOut) ->
    ?LOG_NOTICE("Input:~p MsgState:~p", [Input, MsgState]),
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

simple_test() ->
    SampleList = [
        "INFO hello",
        "some text",
        eof
    ],

    FinalList = [
        "INFO hello",
        "some text"
    ],

    Out = read_lines_list_tester(SampleList, "hello"),

    ?assertEqual(FinalList, Out),
    ok.

simple2_test() ->
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

    Out = read_lines_list_tester(SampleList, "hello"),

    ?assertEqual(FinalList, Out),
    ok.
