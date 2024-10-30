:- module(elf, [run/1, run_codes/2, run_string/2, run_string_pretty/2]).
:- use_module(library(dcg/basics)).
:- use_module(library(yall)).
:- use_module(stdlib/fmt).
:- set_prolog_flag(double_quotes, codes).
:- set_prolog_flag(elf_debug, false).

debug(Term) :- current_prolog_flag(elf_debug, D), debug(D, Term).
debug(false, _).
debug(true, T) :- writeln(T).

ws --> "#", string_without("\n", _), ws.
ws --> [W], { code_type(W, space) }, ws.
ws --> [].

% At least one whitespace
ws1 --> [W], { !, code_type(W, space) }, ws.
ws1 --> ws, [W], { !, code_type(W, space) }.

%% toplevel
%% statements (list of expressions)
%% assignment "<name>: <stmt>"
%% parenthes "(" <statements> ")"

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
mcall(mcall(Name, Args)) --> symbol(Name), "(", ws, mcall_args(Args), ")".
assign(assign(Name, Expr)) --> symbol(Name), ":", ws, exprt(Expr,",").
mcall_args([A|Args]) --> exprt(A, ",)"), ws, more_mcall_args(Args).
more_mcall_args([]) --> [].
more_mcall_args(Args) --> ",", ws, mcall_args(Args).


fun(fun(Arity, Statements)) --> "\\", statements(Statements),
                                { %walk(Statements, arg(Arity)) -> true; Arity = 0
                                    Arity = 0 % fixme
                                }.
lst(list(Items)) --> "[", list_items(Items), ws, "]".
list_items([]) --> [].
list_items([I|Items]) --> ws, exprt(I, ",]"), more_list_items(Items).
more_list_items([]) --> [].
more_list_items(Items) --> ws, ",", list_items(Items).

op_(op(*)) --> "*".
op_(op(/)) --> "/".
op_(op(+)) --> "+".
op_(op(-)) --> "-".
op_(op(>)) --> ">".
op_(op(<)) --> "<".
op_(op(=)) --> "=".
op_(op('%')) --> "%".
op_(op('++')) --> "++". % append lists

% check end, but don't consume it
end(End), [Ch] --> [Ch], { memberchk(Ch, End) }.
end(_) --> ws, eos. % whitespace at the end also closes statements

expr(A) --> value(A); symbol(A); arg_(A); lst(A); mref(A); prev(A); fun(A); op_(A); mcall(A); assign(A).
expr(sub(S)) --> "(", exprt(S, ")"), ")".

exprs([],End) --> end(End).
exprs([E|Exprs], End) -->
    expr(E), more_exprs(Exprs,End).
more_exprs([], End) --> ws, end(End).
more_exprs(Exprs,End) --> ws1, exprs(Exprs,End).

% Take expression parts and turn it into execution tree
exprt(E, End) --> ws, exprs(Es, End), { exprs_tree(Es, E) }.
exprs_tree(Lst, op(Left, Op, Right)) :-
    % Split by binary ops
    once(append([Before, [op(Op)], After], Lst)),
    exprs_tree(Before, Left),
    exprs_tree(After, Right).
exprs_tree(Lst, Lst) :- \+ memberchk(op(_), Lst).

statements([Expr|Stmts]) -->
    ws, exprt(Expr, ",."),
    more_statements(Stmts).
more_statements([]) --> "."; eos.
more_statements(Stmts) --> ",", statements(Stmts).


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



% Functions and method references are callable
is_callable(fun(_,_)).
is_callable(mref(_)).

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
dumpstate --> state(S), {debug(state(S))}.

fail_nil(X) :- dif(X, nil).

eval([Expr|Methods], Out) --> eval(Expr, Val), eval_methods(Val, Methods, Out).

eval(sub(Expr), Out) --> eval(Expr, Out).
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
eval_op(X,'=',X,true).
eval_op(L,'=',R,false) :- dif(L,R).
eval_op(L,'++',R,Append) :- is_list(L), is_list(R), append(L,R,Append).
eval_op(L,'++',R,[L|R]) :- \+ is_list(L), is_list(R).
eval_op(L,'++',R,Append) :- is_list(L), \+ is_list(R), append(L,[R],Append).

eval_methods(In, [], In) --> [].
eval_methods(In, [M|Methods], Out) -->
    eval_method(In, M, Intermediate),
    eval_methods(Intermediate, Methods, Out).

eval_method(In, mcall(Name, Args), Out) -->
    eval_all(Args, ArgValues),
    method(Name, In, ArgValues, Out).
eval_method(In, Name, Out) -->
    { atom(Name) },
    method(Name, In, [], Out).

eval_all([],[]) --> [].
eval_all([In|Ins], [Out|Outs]) --> eval(In, Out), {debug(arg(In,Out))}, eval_all(Ins,Outs).

eval_call(fun(_Arity, Stmts), Args, Result) -->
    %{ debug(call_fn_ars(Arity,Stmts,Args)), length(Args, ArgC),
    %  (Arity = ArgC) -> true; throw(wrong_arity(expected(Arity),args(ArgC))) },
    push_env,
    setargs(Args),
    dumpstate,
    eval_stmts(nil, Stmts, Result), {debug(fn_result(Result))},
    pop_env.

eval_call(mref(Name), [Me|Args], Result) -->
    { length(Args, ArgC),
      method(Name/Argc) -> true; throw(no_such_method_error(name(Name),arity(ArgC))) },
    method(Name, Me, Args, Result).

eval_stmts(Result,[],Result) --> [].
eval_stmts(Prev, [Stmt|Stmts], Result) -->
    setprev(Prev),
    { debug(evaling(Stmt, prev(Prev))) },
    eval(Stmt, Intermediate),
    { debug(intermed(Intermediate)) },
    eval_stmts(Intermediate, Stmts, Result).

eval_if(_, Then, Then) --> [], { \+ is_callable(Then) }.
eval_if(Bool, Then, Result) --> { is_callable(Then) },
                                eval_call(Then, [Bool], Result).

method(if, nil, [_], nil) -->  [].
method(if, false, [_], nil) -->  [].
method(if, Bool, [Then], Result) -->
    { \+ falsy(Bool) },
    eval_if(Bool, Then, Result).

% takes an array of truth values and array of actions
% runs/returns value corresponding to first true value
method(cond, [], [Else], Result) -->
    eval_if(true, Else, Result).
method(cond, [], [], nil) --> [].
method(cond, [B|Bools], [_|Actions], Result) -->
    { falsy(B) },
    method(cond, Bools, Actions, Result).
method(cond, [B|_], [A|_], Result) -->
    { \+ falsy(B) },
    eval_if(B, A, Result).

method(keep, [], [_], []) --> [].
method(keep, [H|T], [Fn], Result) -->
    eval_call(Fn, [H], Hv),
    method(keep, T, [Fn], Tvs),
    { \+ falsy(Hv) -> Result=[Hv|Tvs]; Result=Tvs }.
method(map, [], _, []) --> [].
method(map, [H|T], [Fn], [Hv|Tvs]) -->
    eval_call(Fn, [H], Hv),
    method(map, T, [Fn], Tvs).
method(do, [], _, nil) --> [].
method(do, [H|T], [Fn], nil) -->
    eval_call(Fn, [H], _),
    method(do, T, [Fn], nil).
method(filter, [], _, []) --> [].
method(filter, [H|T], [Fn], Result) -->
    eval_call(Fn, [H], Include),
    method(filter, T, [Fn], Rest),
    { \+ falsy(Include) -> Result=[H|Rest]; Result=Rest }.

% Any pure Prolog method, that doesn't need DCG evaluation context
method(Method, Me, Args, Result) --> [], { method(Method, Me, Args, Result) }.

method(digit, N, [], nil) :- \+ between(48, 57, N).
method(digit, N, [], D) :- between(48,57,N), D is N - 48.
method(print, Me, _, Me) :- fmt:pretty(Me).
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
method(join, Lst, [Sep], Out) :-
    foldl({Sep}/[Item,Acc,Out]>>append([Acc, Sep, Item], Out),
          Lst, [], Intermediate),
    % Remove the separator before first element
    writeln(int(Intermediate)),
    append(Sep, Out, Intermediate).
method(split, Lst, [Sep], Result) :-
    once(append([Start, Sep, Rest], Lst))
    -> (method(split, Rest, [Sep], RestSplit),
        Result=[Start|RestSplit])
    ; Result=Lst.
method(len, Lst, [], Result) :- length(Lst, Result).

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
method(filter/1).
method(join/1).
method(split/1).
method(if/1).
method(if/2).
method(len/0).
method(cond/_).

falsy(nil).
falsy(false).

%% Top level runner interface

run(File) :-
    once(phrase_from_file(stmts(Stmts), File)),
    %debug(got(Stmts)),
    phrase(eval_stmts(nil,Stmts,_Res), [ctx(env{},[],nil)], _).

run_codes(Input, Out) :-
    once(phrase(statements(Stmts), Input)),
    debug(parsed_program(Stmts)),
    phrase(eval_stmts(nil,Stmts,Out), [ctx(env{},[],nil)], _).

run_string(Input, Out) :-
    string_codes(Input, Codes),
    run_codes(Codes, Out).

run_string_pretty(Input, Out) :-
    catch((run_string(Input, Result),
           with_output_to(string(Out),
                          fmt:pretty(Result))),
          Err,
          Out = naughty(Err)).


:- begin_tests(elf).
:- set_prolog_flag(double_quotes, codes).

prg("foo: \"jas6sn0\", foo keep(&digit).", [6,0]).
prg("[4, 2, 0, 6, 9] sum", 21).
prg("[6, 2] sum * 4", 32).
prg("\"README.md\" lines first", "# Elf Helper programming language").
prg("[1, 2, 3, 4] map(\\ $ * 2.)", [2, 4, 6, 8]).
prg("[1, 2, 3, 4] heads", [[1,2,3,4],[2,3,4],[3,4],[4]]).
prg("\"elf\" reverse", "fle").
prg("\"some4digt02here\" keep(&digit), (_ first * 10) + _ last", 42).
prg("1 to(5) reverse", [5,4,3,2,1]).
prg("42 to(69, 7)", [42,49,56,63]).
prg("\"%s day with %d%% chance of rain\" fmt(\"sunny\",7)",
    "sunny day with 7% chance of rain").
prg("[\"foo\", \"bar\" ,\"baz\"] join(\" and \")",
    "foo and bar and baz").
prg("\"foo, quux, !\" split(\", \")",
    ["foo", "quux", "!"]).
prg("(\"foo\" ++ \"bar\") len", 6).
prg("([\"foo\" len] first > 1) if(\"big\")", "big").

test(programs, [forall(prg(Source,Expected))]) :-
    once(run_codes(Source,Actual)),
    Expected = Actual.


:- end_tests(elf).
