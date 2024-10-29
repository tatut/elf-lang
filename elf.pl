:- module(elf, [run/1, run_codes/2]).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- use_module(stdlib/fmt).
:- set_prolog_flag(double_quotes, codes).

ws --> "#", string_without("\n", _), ws.
ws --> [W], { code_type(W, space) }, ws.
ws --> [].

% At least one whitespace
ws1 --> [W], { !, code_type(W, space) }, ws.
ws1 --> ws, [W], { !, code_type(W, space) }.

value(val(N)) --> number(N).
value(val(S)) --> "\"", string_without("\"", S), "\"".
value(val(Ch)) --> "@", [Ch].
value(val(true)) --> "true".
value(val(false)) --> "false".
value(val(nil)) --> "nil".
symbol(Name) --> csym(Name), { dif(Name, '_') }.
arg_(arg(1)) --> "$".
arg_(arg(N)) --> "$", integer(N).
prev(prevval) --> "_".
mref(mref(Name)) --> "&", csym(Name).
fun(fun(Arity, Statements)) --> "\\", stmts(Statements),
                                { walk(Statements, arg(Arity)) -> true; Arity = 0}.
lst(list(Items)) --> "[", lst_items(Items), ws, "]".
lst_items([]) --> [].
lst_items([I|Items]) --> ws, expr(I),lst_items(Items).

%% Walk statements, find items
walk([First|_], Item) :- walk(First, Item).
walk([_|Items], Item) :- walk(Items, Item).
walk(stmt(Target,_Methods), Target).
walk(stmt(_, Methods), Item) :- walk(Methods, Item).
walk(method(Item,_Args), Item).
walk(method(_,Args), Item) :- walk(Args, Item).
walk(fun(_, Statements), Item) :- walk(Statements, Item).
walk(op(Left,_Op,Right), Item) :- walk(Left,Item); walk(Right,Item).
walk(Item, Item).

expr(A) --> value(A); symbol(A); arg_(A); lst(A); mref(A); prev(A); fun(A).
op_(*) --> "*".
op_(/) --> "/".
op_(+) --> "+".
op_(-) --> "-".
op_(>) --> ">".
op_(<) --> "<".
op_(=) --> "=".
op_('%') --> "%".


stmt(stmt(Target,Methods)) -->
    expr(Target),
    methods(Methods).
stmt(assign(Name, Value)) -->
    expr(Value), ws, "->", ws, csym(Name).
stmt(op(Left,Op,Right)) --> stmt_left(Left), ws, op_(Op), ws, stmt(Right), ws.
stmt_left(S) --> "(", ws, stmt(S), ws, ")".
stmt_left(S) --> stmt(S).

stmts([St|Stmts]) --> ws, stmt(St), ws, more_stmts(Stmts).
more_stmts([]) --> "."; eos.
more_stmts([St|Stmts]) --> ",", ws, stmt(St), more_stmts(Stmts).

methods([]) --> [].
methods([M|Ms]) --> ws1, method(M), methods(Ms).
method(method(Sym,[])) --> symbol(Sym).
method(method(Sym,Arguments)) --> symbol(Sym), "(", method_args(Arguments), ")".

method_args([Arg|Args]) --> stmt(Arg), more_method_args(Args).
more_method_args([]) --> [].
more_method_args([Arg|Args]) --> ws1, stmt(Arg), more_method_args(Args).


%%%% Evaluator

% The evaluation state is a compound term
%
% ctx(env{foo: 42}, Args, Prev).
% Where env is a dictionary containing lexical bindings
% and Args is the current list of arguments to fn call
% Prev is the previous result referred to by _

% DCG state
state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

push_env, [S, Env] --> [S], { S = ctx(Env,_,_) }.
pop_env, [ctx(EnvSaved,[],nil)] --> [ctx(_,_,_), EnvSaved].
setprev(P) --> state(ctx(E,A,_), ctx(E,A,P)).
setargs(A) --> state(ctx(E,_,P), ctx(E,A,P)).
dumpstate --> state(S), {writeln(state(S))}.

fail_nil(X) :- dif(X, nil).

eval(stmt(Expr, []), Val) --> eval(Expr, Val).
eval(stmt(Expr, [M|Methods]), Val) -->
    eval(Expr, In),
    eval_methods(In, [M|Methods], Val).
eval(Sym, Val) --> { atom(Sym) },
                   state(ctx(Env,_,_)),
                   { get_dict(Sym, Env, Val) }.
eval(assign(Name, Expr), Val) -->
    eval(Expr, Val),
    state(ctx(Env0,A,P), ctx(Env1,A,P)),
    { put_dict(Name, Env0, Val, Env1) }.
eval(val(V), V) --> [].
eval(list([]), []) --> [].
eval(list([H|T]), [Hv|Tvs]) -->
    eval(H, Hv),
    eval(list(T), Tvs).
eval(mref(Name), mref(Name)) --> [].
eval(prevval, Val) --> state(ctx(_,_,Val)).
eval(op(Left,Op,Right), Val) -->
    eval(Left, Lv),
    eval(Right, Rv),
    { eval_op(Lv,Op,Rv, Val) }.
eval(fun(A,S), fun(A,S)) --> [].
eval(arg(N), V) --> state(ctx(_,Args,_)), { nth1(N, Args, V) }.

eval_op(L,+,R,V) :- V is L + R.
eval_op(L,-,R,V) :- V is L - R.
eval_op(L,*,R,V) :- V is L * R.
eval_op(L,/,R,V) :- V is L / R.
eval_op(L,>,R,false) :- L =< R.
eval_op(L,>,R,true) :- L > R.
eval_op(L,<,R,false) :- L >= R.
eval_op(L,<,R,true) :- L < R.
eval_op(L,'%',R,V) :- V is L mod R.

eval_methods(In, [], In) --> [].
eval_methods(In, [M|Methods], Out) -->
    eval_method(In, M, Intermediate),
    eval_methods(Intermediate, Methods, Out).

eval_method(In, method(Name, Args), Out) -->
    eval_all(Args, ArgValues),
    method(Name, In, ArgValues, Out).

eval_all([],[]) --> [].
eval_all([In|Ins], [Out|Outs]) --> eval(In, Out), {writeln(arg(In,Out))}, eval_all(Ins,Outs).

eval_call(fun(Arity, Stmts), Args, Result) -->
    { writeln(call_fn_ars(Arity,Stmts,Args)), length(Args, ArgC),
      (Arity = ArgC) -> true; throw(wrong_arity(expected(Arity),args(ArgC))) },
    %eval_all(Args, ArgVals),{writeln(argvals(ArgVals))},
    %^fixme: already evaled
    push_env,
    setargs(Args),
    dumpstate,
    eval_stmts(nil, Stmts, Result), {writeln(fn_result(Result))},
    pop_env.

eval_call(mref(Name), [Me|Args], Result) -->
    { length(Args, ArgC),
      method(Name/Argc) -> true; throw(no_such_method_error(name(Name),arity(ArgC))) },
    method(Name, Me, Args, Result).

eval_stmts(Result,[],Result) --> [].
eval_stmts(Prev, [Stmt|Stmts], Result) -->
    setprev(Prev),
    { writeln(evaling(Stmt, prev(Prev))) },
    eval(Stmt, Intermediate),
    { writeln(intermed(Intermediate)) },
    eval_stmts(Intermediate, Stmts, Result).

method(keep, [], [_], []) --> [].
method(keep, [H|T], [Fn], Result) -->
    eval_call(Fn, [H], Hv),
    method(keep, T, [Fn], Tvs),
    { \+ falsy(Hv) -> Result=[Hv|Tvs]; Result=Tvs }.
method(map, [], _, []) --> [].
method(map, [H|T], [Fn], [Hv|Tvs]) -->
    eval_call(Fn, [H], Hv),
    method(map, T, [Fn], Tvs).

% Any pure Prolog method, that doesn't need DCG evaluation context
method(Method, Me, Args, Result) --> [], { method(Method, Me, Args, Result) }.

method(digit, N, [], nil) :- \+ between(48, 57, N).
method(digit, N, [], D) :- between(48,57,N), D is N - 48.
method(print, Me, _, Me) :- writeln(Me).
method(sum, Lst, [], Result) :- sum_list(Lst, Result).
method(first, [H|_], [], H).
method(last, Lst, [], Last) :- last(Lst, Last).
method(nth, Lst, [N], Nth) :- nth0(N, Lst, Nth).
method(lines, File, [], Lines) :-
    atom_codes(F, File),
    read_file_to_string(F, Str,[]),
    string_lines(Str, LinesStr),
    maplist(string_codes, LinesStr, Lines).
method(heads, [], [], []).
method(heads, [H|T], [], [[H|T]|Heads]) :- method(heads, T, [], Heads).
method(reverse, Lst, [], Rev) :- reverse(Lst,Rev).
method(to, From, [To], Lst) :- findall(N, between(From,To,N), Lst).
method(to, From, [To, _], []) :- From > To.
method(to, From, [To, Inc], [From|Rest]) :- From1 is From + Inc, method(to, From1, [To, Inc], Rest).
method(fmt, PatternCs, Args, Out) :-
    fmt:fmt(PatternCs, Args, Out).

% add all methods here
method(keep/1).
method(print/0).
method(digit/0).
method(map/1).
method(sum/0).
method(first/0).
method(last/0).
method(nth/0).
method(lines/0).
method(heads/0).
method(reverse/0).
method(to/1).
method(to/2).

falsy(nil).
falsy(false).

%% Top level runner interface

run(File) :-
    once(phrase_from_file(stmts(Stmts), File)),
    %writeln(got(Stmts)),
    phrase(eval_stmts(nil,Stmts,_Res), [ctx(env{},[],nil)], _).

run_codes(Input, Out) :-
    once(phrase(stmts(Stmts), Input)),
    writeln(got(Stmts)),
    phrase(eval_stmts(nil,Stmts,Out), [ctx(env{},[],nil)], _).


:- begin_tests(elf).
:- set_prolog_flag(double_quotes, codes).

prg("\"jas6sn0\" -> foo, _ keep(&digit).", [6,0]).
prg("[4 2 0 6 9] sum", 21).
prg("[6 2] sum * 4", 32).
prg("\"README.md\" lines first", "# Elf Helper programming language").
prg("[1 2 3 4] map(\\ $ * 2.)", [2, 4, 6, 8]).
prg("[1 2 3 4] heads", [[1,2,3,4],[2,3,4],[3,4],[4]]).
prg("\"elf\" reverse", "fle").
prg("\"some4digt02here\" keep(&digit), (_ first * 10) + _ last", 42).
prg("1 to(5) reverse", [5,4,3,2,1]).
prg("42 to(69 7)", [42,49,56,63]).
prg("\"%s day with %d%% chance of rain\" fmt(\"sunny\" 7)",
    "sunny day with 7% chance of rain").

test(programs, [forall(prg(Source,Expected))]) :-
    once(run_codes(Source,Actual)),
    Expected = Actual.


:- end_tests(elf).
