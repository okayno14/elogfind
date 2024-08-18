-module(elogfind).

%% API exports
-export([main/1]).

-define(LOG_LEVEL_LABEL_LIST, [
    "ERROR", "WARN", "INFO", "TRACE", "DEBUG"
]).

%%====================================================================
%% API functions
%%====================================================================

%% TODO 1 file name, 1 LineTarget
%% TODO сделать аргумент для запуска тестов
%% escript Entry point
main(_Args) ->
    {ListOut, MsgState} = fsm_begin("INFO hello", "hello"),
    io:format("~p ~p~n", [ListOut, MsgState]),
    {ListOut2, MsgState2} = fsm({input, eof}, "hello", MsgState, ListOut),
    io:format("~p ~p~n", [ListOut2, MsgState2]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

-type msg_state() :: {print | noprint, last | nolast, MsgAcc :: list(string())}.
-type out() :: {print, MsgAcc :: list(string())} | noprint.
-type line_input() :: string() | eof.

fsm_begin(Line, LineTarget) ->
    fsm({input, Line}, LineTarget, {noprint, nolast, []}, []).

%%--------------------------------------------------------------------
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
    {[out()], msg_state()}.
%%--------------------------------------------------------------------
%% Добавить Line в MsgAcc, сменить или оставить nolast
%% init

fsm({input, eof}, LineTarget, {Print, nolast, MsgAcc}, ListOut) ->
    fsm({out, eof}, LineTarget, {Print, last, MsgAcc}, ListOut);

fsm({input, Line}, LineTarget, {noprint, nolast, []}, ListOut = []) ->
    case log_begins(Line) of
        true ->
            fsm({check, Line}, LineTarget, {noprint, nolast, [Line]}, ListOut);

        false ->
            fsm({out, Line}, LineTarget, {noprint, last, [Line]}, ListOut)
    end;

fsm({input, Line}, LineTarget, {noprint, nolast, MsgAcc}, ListOut) ->
    case log_begins(Line) of
        true ->
            fsm({check, Line}, LineTarget, {noprint, last, [Line | MsgAcc]}, ListOut);

        false ->
            fsm({check, Line}, LineTarget, {noprint, nolast, [Line | MsgAcc]}, ListOut)
    end;

fsm({input, Line}, LineTarget, {print, nolast, MsgAcc}, ListOut) ->
    case log_begins(Line) of
        true ->
            fsm({out, Line}, LineTarget, {print, last, [Line | MsgAcc]}, ListOut);

        false ->
            {lists:reverse(ListOut), {print, nolast, [Line | MsgAcc]}}
    end;

%% Сменить или оставить noprint
fsm({check, Line}, LineTarget, MsgState = {noprint, nolast, MsgAcc}, ListOut) ->
    case match_target(Line, LineTarget) of
        true ->
            {lists:reverse(ListOut), {print, nolast, MsgAcc}};

        false ->
            {lists:reverse(ListOut), MsgState}
    end;

fsm({check, Line}, LineTarget, MsgState = {noprint, last, MsgAcc}, ListOut) ->
    case match_target(Line, LineTarget) of
        true ->
            fsm({out, Line}, LineTarget, {print, last, MsgAcc}, ListOut);

        false ->
            fsm({out, Line}, LineTarget, MsgState, ListOut)
    end;

%% сменить ListOut
%% pre last
fsm({out, eof}, _LineTarget, {noprint, last, _MsgAcc}, ListOut) ->
    {lists:reverse([noprint | ListOut]), {noprint, nolast, []}};

fsm({out, eof}, _LineTarget, {print, last, MsgAcc}, ListOut) ->
    ListOut2 = [{print, lists:reverse(MsgAcc)} | ListOut],
    {ListOut2, {noprint, nolast, []}};

%% bad path - дропнуть MsgAcc, дропнуть Line, выйти в receive
fsm({out, _Line}, _LineTarget, {noprint, last, _MsgAcc}, ListOut) ->
    {lists:reverse([noprint | ListOut]), {noprint, nolast, []}};

%% good path - переложить MsgAcc в ListOut, переложить Line в input
fsm({out, Line}, LineTarget, {print, last, MsgAcc}, ListOut) ->
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

