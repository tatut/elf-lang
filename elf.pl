:- module(elf, [run/1, run_codes/2, run_string/2, run_string_pretty/2, repl/0]).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/high_order)).
:- use_module(library(yall)).
:- use_module(library(readutil)).
:- use_module(elf_fmt).
:- use_module(elf_record).
:- use_module(elf_map).
:- use_module(elf_file).
:- use_module(elf_ref).
:- set_prolog_flag(double_quotes, codes).
:- set_prolog_flag(elf_use_fetch, false).
:- set_prolog_flag(stack_limit, 2_147_483_648).

%:- det(method/4).

version('0.3').

% Reserved words that cannot be names.
reserved(and).
reserved(or).
reserved('_').

% Print error and fail
err(Fmt, Args) :- format(string(Err), Fmt, Args), writeln(user_error, Err), throw(err(Fmt,Args)).

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
value(val(regex(R))) --> "`", string_without("`", S), "`", { string_codes(Str, S), re_compile(Str, R, []) }.

symbol_chf(Ch) --> [Ch], { code_type(Ch, alpha) }.
symbol_chf(95) --> [95]. % _
symbol_ch(Ch) --> [Ch], { symbol_ch(Ch) }.
symbol_ch(Ch) :- code_type(Ch, alnum).
symbol_ch(95). % _
symbol_ch(45). % -
symbol_ch(63). % ?

symbol_(Name) --> symbol_chf(Ch), sequence(symbol_ch, Chs),
                  { atom_codes(Name, [Ch|Chs]) }.

symbol(sym(Name)) --> symbol_(Name), { \+ reserved(Name) }.
arg_(arg(1)) --> "$".
arg_(arg(N)) --> "$", integer(N).
prev(prevval) --> "_".
ref(R) --> symbol(R); arg_(R); prev(R).

mref(mref(Name)) --> "&", csym(Name).
mref(mref(Op)) --> "&", op_(Op).

mcall(mcall(Name, Args)) --> symbol_(Name), "(", ws, mcall_args(Args), ")".
assign(assign(Name, Expr)) --> symbol_(Name), ":", ws, exprt(Expr).
assign(assign(Name1, [Name2], Expr)) --> symbol_(Name1), ".", symbol_(Name2), ":",
                                         ws, exprt(Expr).

mcall_args([A|Args]) --> exprt(A), ws, more_mcall_args(Args).
more_mcall_args([]) --> [].
more_mcall_args(Args) --> ",", ws, mcall_args(Args).
record_def(record_def(Name,Fields,Methods)) --> symbol_(Name), "{", record_def_fields(Fields), record_def_methods(Methods), "}".
record_def_fields([]) --> ws.
record_def_fields([F|Fields]) --> ws, symbol_(F), more_record_def_fields(Fields).
more_record_def_fields([]) --> ws.
more_record_def_fields(Fields) --> ws, ",", record_def_fields(Fields).

record_def_methods([]) --> ws.

record_(record(Name, Fields)) --> symbol_(Name), "{", record_fields(Fields), "}".
record_fields([]) --> ws.
record_fields([F-V|Fields]) --> ws, symbol_(F), ":", ws, exprt(V), more_record_fields(Fields).
more_record_fields([]) --> ws.
more_record_fields(Fields) --> ws, ",", record_fields(Fields).

map_new(map_new(Fields)) --> "%{", map_fields(Fields), "}".
map_fields([]) --> ws.
map_fields([F-V|Fields]) --> ws, exprt(F), ":", ws, exprt(V), more_map_fields(Fields).
more_map_fields([]) --> ws.
more_map_fields(Fields) --> ws, ",", map_fields(Fields).


fun(fun(ArgNames, Statements)) -->
    "{", optional(fun_argnames(ArgNames), {ArgNames=[]}),
    statements(Statements), "}".

% Argument names: comma separated symbols ending with |
fun_argnames(ArgNames) --> sequence(ws, symbol_, (ws, ",", ws), (ws, "|"), ArgNames).

lst(list(Items)) --> "[", list_items(Items), ws, "]".
list_items([]) --> [].
list_items([I|Items]) --> ws, exprt(I), more_list_items(Items).
more_list_items([]) --> [].
more_list_items(Items) --> ws, ",", list_items(Items).

op_(op(*)) --> "*".
op_(op(/)) --> "/".
op_(op(+)) --> "+".
op_(op(-)) --> "-".
op_(op(>)) --> ">".
op_(op(>=)) --> ">=".
op_(op(<)) --> "<".
op_(op(<=)) --> "<=".
op_(op(=)) --> "=".
op_(op('%')) --> "%".
op_(op('++')) --> "++". % append lists
op_(op(and)) --> "and".
op_(op(or)) --> "or".

% check end, but don't consume it
end(End), [Ch] --> [Ch], { memberchk(Ch, End) }.
end(_) --> ws, eos. % whitespace at the end also closes statements

expr(A) --> value(A); symbol(A); arg_(A); lst(A); mref(A); prev(A); fun(A); op_(A); mcall(A); record_def(A); record_(A); map_new(A).
expr(sub(S)) --> "(", exprt(S), ")".

exprs([]) --> [].
exprs([E|Exprs]) -->
    expr(E), more_exprs(Exprs).
more_exprs([]) --> ws.
more_exprs(Exprs) --> ws1, exprs(Exprs).

% Take expression parts and turn it into execution tree
exprt(E) --> ws, exprs(Es), { exprs_tree(Es, E) }.
exprs_tree(Lst, op(Left, Op, Right)) :-
    % Split by binary ops
    once(append([Before, [op(Op)], After], Lst)),
    exprs_tree(Before, Left),
    exprs_tree(After, Right).
exprs_tree(Lst, Lst) :- \+ memberchk(op(_), Lst).

stmt(St) --> exprt(St).
stmt(St) --> assign(St).

statements([Expr|Stmts]) -->
    ws, stmt(Expr),
    more_statements(Stmts).
more_statements([]) --> ws; eos.
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



% Functions, method references and regexes are callable
is_callable(fun(_,_)).
is_callable(mref(_)).
is_callable(regex(_)).

%%%% Evaluator

% The evaluation state is a compound term
%
% ctx(env{foo: 42}, Args, Prev).
% Where env is a dictionary containing lexical bindings
% and Args is the current list of arguments to fn call
% Prev is the previous result referred to by _

builtin_class('List').
builtin_class('Number').
builtin_class('Nil').
builtin_class('Boolean').

class(rec(C,_), C) :- !.
class(nil, 'Nil') :- !.
class(false, 'Boolean') :- !.
class(true, 'Boolean') :- !.
class(X, 'List') :- is_list(X), !.
class(X, 'Number') :- number(X), !.


% DCG state
state(S), [S] --> [S].
state(S0, S), [S] --> [S0].

push_env, [S, S] --> [S].
pop_env, [CtxSaved] --> [ctx(_,_,_), CtxSaved].
setprev(P) --> state(ctx(E,A,_), ctx(E,A,P)).
setargs(A) --> state(ctx(E,_,P), ctx(E,A,P)).
setenv(Name,Val) --> state(ctx(Env0,A,P), ctx(Env1,A,P)),
                     { put_dict(Name, Env0, Val, Env1) }.
getenv(Name,record_ref(Name)) --> [], { once(record_field(Name,_,_)), ! }.
getenv(Name,Val) --> state(ctx(Env,_,_)),
                     { (get_dict(Name, Env, Val), !); err('Name error: ~w', [Name]) }.

eval([Expr|Methods], Out) --> eval(Expr, Val), eval_methods(Methods, Val, Out).
eval(sub(Expr), Out) --> eval(Expr, Out).
eval(sym(Sym), Val) --> getenv(Sym,Val).
eval(assign(Name, Expr), Val) -->
    eval(Expr, Val),
    state(ctx(Env0,A,P), ctx(Env1,A,P)),
    { put_dict(Name, Env0, Val, Env1) }.

% Assign new method to record
eval(assign(At, [Path], Expr), Val) -->
    { once((record_field(At,_,_);builtin_class(At))) },
    eval(Expr, Val),
    { asserta(record_method(Path, At, Val)) }.

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
    { once(eval_op(Op,Lv,Rv, Val)) }.
eval(fun(A,S), fun(A,S)) --> [].
eval(arg(N), V) --> state(ctx(_,Args,_)), { nth1(N, Args, V) }.

eval(record_def(Name, Fields, _Methods), record_ref(Name)) -->
    [],
    { length(Fields, Len),
      findall(I, between(1,Len,I), Is),
      maplist({Name,Fields}/[I]>>(nth1(I, Fields, F),
                                  asserta(record_field(Name, I, F))), Is) }.

eval(record(Name, Fields), R1) -->
    { record_new(Name, R0) },
    eval_record_fields(Fields, Name, R0, R1).

eval(map_new(Fields), Map) -->
    { map_new(Map0) },
    eval_map_fields(Fields, Map0, Map).

eval_map_fields([], M, M) --> [].
eval_map_fields([KeyExpr-ValExpr|Fields], Map0, MapOut) -->
    eval(KeyExpr, Key),
    eval(ValExpr, Val),
    { map_put(Map0, Key, Val, Map1) },
    eval_map_fields(Fields, Map1, MapOut).

eval_record_fields([], _, R, R) --> [].
eval_record_fields([Name-ValExpr|Fields], Record, R0, R) -->
    eval(ValExpr, Val),
    { record_set(R0, Name, Val, R1) },
    eval_record_fields(Fields, Record, R1, R).

lift('++') :- !, fail.
lift('=') :- !, fail.
lift(_).

eval_op_rl(Op, R, L, V) :- eval_op(Op, L, R, V).
eval_op(Op, L, R, V) :- lift(Op), is_list(L), \+ is_list(R), maplist(eval_op_rl(Op,R), L, V), !.
eval_op(Op, L, R, V) :- lift(Op), is_list(L), is_list(R), maplist(eval_op(Op), L, R, V), !.
eval_op(+,L,R,V) :- V is L + R, !.
eval_op(-,L,R,V) :- V is L - R, !.
eval_op(*,L,R,V) :- V is L * R, !.
eval_op(/,L,R,V) :- V is L / R, !.
eval_op(>,L,R,false) :- L =< R, !.
eval_op(>,L,R,true) :- L > R, !.
eval_op(<,L,R,false) :- L >= R, !.
eval_op(<,L,R,true) :- L < R, !.
eval_op(<=,L,R,true) :- L =< R, !.
eval_op(<=,L,R,false) :- L > R, !.
eval_op(>=,L,R,true) :- L >= R, !.
eval_op(>=,L,R,false) :- L < R, !.
eval_op('%',L,R,V) :- V is L mod R, !.
eval_op('=',X,X,true) :- !.
eval_op('=',_,_,false) :- !.
eval_op('++',L,R,Append) :- is_list(L), is_list(R), append(L,R,Append), !.
% nil acts as empty list for append
eval_op('++',nil,R,R) :- !.
eval_op('++',L,nil,L) :- !.
eval_op(and,true,true,true) :- !.
eval_op(and,false,false,false) :- !.
eval_op(and,true,false,false) :- !.
eval_op(and,false,true,false) :- !.
eval_op(and,L,R, true) :- \+ falsy(L), \+ falsy(R), !.
eval_op(and,L,_, false) :- falsy(L), !.
eval_op(and,_,R, false) :- falsy(R), !.
eval_op(or,true,true,true) :- !.
eval_op(or,true,false,true) :- !.
eval_op(or,false,true,true) :- !.
eval_op(or,false,false,false) :- !.

eval_methods([], In, In) --> [].
eval_methods([M|Methods], In , Out) -->
    eval_method(M, In, Intermediate),
    { ! }, % commit to this method call result (don't leave choicepoints around)
    eval_methods(Methods, Intermediate, Out).

eval_method(mcall(Name, Args), In, Out) -->
    eval_all(Args, ArgValues),
    method(Name, In, ArgValues, Out), { ! }.
eval_method(sym(Name), In, Out) --> method(Name, In, [], Out), { ! }.
eval_method(mcall(Name,Args), _In, _Out) -->
    { length(Args,Arity), err('Method call failed: ~w/~w', [Name, Arity]) }.
eval_method(sym(Name), _In, _Out) -->
    { err('Method call failed: ~w/0', [Name]) }.

eval_all([],[]) --> [].
eval_all([In|Ins], [Out|Outs]) --> eval(In, Out), eval_all(Ins,Outs).

eval_call(fun(ArgNames, Stmts), Args, Result) -->
    push_env,
    setargs(Args),
    bind_args(ArgNames, Args),
    eval_stmts(Stmts, nil, Result),
    pop_env.

eval_call(mref(op(Op)), [[Left|Right]], Result) -->
    [],
    { eval_op(Op,Left,Right,Result) }.

eval_call(mref(Name), [Me|Args], Result) -->
    { atom(Name) },
    method(Name, Me, Args, Result).

eval_call(regex(R), [Lst], Result) -->
    [],
    { string_codes(Str, Lst),
      re_matchsub(R, Str, Match)
      -> (dict_pairs(Match, re_match, Vals),
          sort(Vals, ValsSorted),
          maplist([_-S,L]>>string_codes(S,L), ValsSorted, Result))
      ; Result=nil }.

bind_args([], _) --> []. % extra arguments, might be $ references, don't care
bind_args([A|Args], []) --> [], { err('Too few arguments provided, missing: ~w', [[A|Args]]) }.
bind_args([N|Names],[V|Values]) --> setenv(N, V), bind_args(Names,Values).

% Eval a method defined on a record
eval_call_my(fun(ArgNames, Stmts), My, Args, Result) -->
    push_env,
    setargs(Args),
    bind_args(ArgNames, Args),
    setenv(my, My),
    eval_stmts(Stmts, nil, Result),
    pop_env.

eval_stmts([],Result,Result) --> [].
eval_stmts([Stmt|Stmts], Prev, Result) -->
    setprev(Prev),
    eval(Stmt, Intermediate),
    eval_stmts(Stmts, Intermediate, Result).

eval_if(_, Then, Then) --> [], { \+ is_callable(Then), ! }.
eval_if(Bool, Then, Result) --> { is_callable(Then), ! },
                                eval_call(Then, [Bool], Result).

foldfn(nil, _, EmptyVal, _, _, _, EmptyVal) --> [].
foldfn([], _, EmptyVal, _, _, _, EmptyVal) --> [].
foldfn([X|Xs], Fn, _, InitialVal, AccumulateGoal, Finalize, Result) -->
    foldfn_([X|Xs], Fn, InitialVal, AccumulateGoal, Finalize, Result).

% variant of fold with witness (the value itself as FnResult-Value pairs)
foldfnw(nil, _, EmptyVal, _, _, _, EmptyVal) --> [].
foldfnw([], _, EmptyVal, _, _, _, EmptyVal) --> [].
foldfnw([X|Xs], Fn, _, InitialVal, AccumulateGoal, Finalize, Result) -->
    foldfnw_([X|Xs], Fn, InitialVal, AccumulateGoal, Finalize, Result).


foldfn_([], _, Cur, _, Finalize, Result) --> [], { call(Finalize, Cur, Result) }.
foldfn_([X|Xs], Fn, Cur, Accumulate, Finalize, Result) -->
    eval_call(Fn, [X], Val),
    { call(Accumulate, Cur, Val, Cur1) },
    foldfn_(Xs, Fn, Cur1, Accumulate, Finalize, Result).

foldfnw_([], _, Cur, _, Finalize, Result) --> [], { call(Finalize, Cur, Result) }.
foldfnw_([X|Xs], Fn, Cur, Accumulate, Finalize, Result) -->
    eval_call(Fn, [X], Val),
    { call(Accumulate, Cur, Val-X, Cur1) },
    foldfnw_(Xs, Fn, Cur1, Accumulate, Finalize, Result).


method(if, nil, [_], nil) -->  [].
method(if, false, [_], nil) -->  [].
method(if, Bool, [Then], Result) -->
    { \+ falsy(Bool) },
    eval_if(Bool, Then, Result).
method(if, nil, [_, Else], Result) --> eval_if(nil, Else, Result).
method(if, false, [_, Else], Result) --> eval_if(false, Else, Result).
method(if, Bool, [Then, _], Result) -->
    { \+ falsy(Bool) },
    eval_if(Bool, Then, Result).

method(while, Fn, [Then], Result) -->
    eval_call(Fn, [], Bool),
    method('%while', Bool-Fn, Then, Result).
method('%while', Bool-_, _, nil) --> { falsy(Bool) }.
method('%while', Bool-Fn, Then, nil) -->
    { \+ falsy(Bool) },
    eval_call(Then, [Bool], _),
    method(while, Fn, [Then], nil).
method(swap, ref(ID), [Fn|Args], New) -->
    { ref_get(ref(ID), Current),
      append([Current],Args, CallArgs) },
    eval_call(Fn, CallArgs, New),
    { ref_set(ref(ID), New) }.

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

method(keep, Lst, [Fn], Result) -->
    foldfn(Lst, Fn, [], [], keep_, reverse, Result).
method(map, Lst, [Fn], Result) -->
    foldfn(Lst, Fn, [], [], map_, reverse, Result).
method(count, Lst, [Fn], Result) -->
    foldfn(Lst, Fn, 0, 0, count_, '=', Result).
method(sort, Lst, [Fn], Result) -->
    foldfnw(Lst, Fn, [], [], map_, keysort, Result0),
    { pairs_values(Result0, Result) }.

method(map, map(M0), [Fn], map(M2)) -->
    { map_new(map(M1)),
      map_pairs(map(M0), Pairs) },
    method('%mapvals'(Fn), map(M1), Pairs, map(M2)).
method('%mapvals'(_), M, [], M) --> [].
method('%mapvals'(Fn), map(M0), [K-V|KVs], map(M2)) -->
    eval_call(Fn, [V], V1),
    { map_put(map(M0), K, V1, map(M1)) },
    method('%mapvals'(Fn), map(M1), KVs, map(M2)).
method(some, nil, _, nil) --> [].
method(some, [], _, nil) --> [].
method(some, [H|T], [Fn], Result) -->
    eval_call(Fn, [H], Res0),
    { falsy(Res0) },
    method(some, T, [Fn], Result).
method(some, [H|_], [Fn], Result) -->
    eval_call(Fn, [H], Result),
    { \+ falsy(Result), ! }.

method('all?', [], [_], true) --> []. % vacuous truth
method('all?', [X|_], [Fn], false) --> eval_call(Fn, [X], Res), { falsy(Res) }.
method('all?', [X|Xs], [Fn], Result) -->
    eval_call(Fn, [X], Res), { \+ falsy(Res) },
    method('all?', Xs, [Fn], Result).

method(mapcat, nil, _, []) --> [].
method(mapcat, [], _, []) --> [].
method(mapcat, [H|T], [Fn], Result) -->
    eval_call(Fn, [H], Hv),
    method(map, T, [Fn], Tvs),
    { append([Hv|Tvs], Result), ! }.

method(sum, Lst, [Fn], Result) --> foldfn(Lst, Fn, 0, 0, plus, '=', Result).

method(min, Lst, [Fn], Result) --> foldfn(Lst, Fn, nil, nil, min_, '=', Result).
method(max, Lst, [Fn], Result) --> foldfn(Lst, Fn, nil, nil, max_, '=', Result).
method(minw, Lst, [Fn], Result) --> foldfnw(Lst, Fn, nil, nil, minw_, pair_list, Result).
method(maxw, Lst, [Fn], Result) --> foldfnw(Lst, Fn, nil, nil, maxw_, pair_list, Result).

method(group, nil, _, M) --> [], { map_new(M) }.
method(group, [], _, M) --> [], { map_new(M) }.
method(group, [X|Xs], [Fn], M1) -->
    { map_new(M0) },
    method('%group'(Fn), M0, [X|Xs], M1).
method('%group'(_), M, [], M) --> [].
method('%group'(Fn), M0, [X|Xs], M2) -->
    eval_call(Fn, [X], Group),
    { map_get(M0, Group, Current),
      eval_op('++',Current,[X], New),
      map_put(M0, Group, New, M1) },
    method('%group'(Fn), M1, Xs, M2).

method('splitw', L, [Fn], [Taken,Dropped]) -->
    method('takew', L, [Fn], Taken),
    method('dropw', L, [Fn], Dropped).

method('takew', [], [_], []) --> [].
method('takew', [X|Xs], [Fn], Result) -->
    eval_call(Fn, [X], Bool),
    method('%takew', [X|Xs], [Fn,Bool], Result).
method('%takew', _, [_, Falsy], []) -->
    [],
    { falsy(Falsy) }.
method('%takew', [X|Xs], [Fn, Truthy], [X|Result]) -->
    { \+ falsy(Truthy) },
    method('takew', Xs, [Fn], Result).
method('dropw', [], [_], []) --> [].
method('dropw', [X|Xs], [Fn], Result) -->
    eval_call(Fn, [X], Bool),
    method('%dropw', [X|Xs], [Fn,Bool], Result).
method('%dropw', Lst, [_, Falsy], Lst) -->
    [],
    { falsy(Falsy) }.
method('%dropw', [_|Xs], [Fn,Truthy], Result) -->
    { \+ falsy(Truthy) },
    method('dropw', Xs, [Fn], Result).

% Fold for nil or empty list is just the initial value (or nil if none specified)
method(fold, [], [_, Init], Init) --> [].
method(fold, nil, [_, Init], Init) --> [].
method(fold, [], [_Fn], nil) --> [].
method(fold, nil, [_Fn], nil) --> [].

method(fold, Lst, Args, Result) -->
    { is_list(Lst), length(Lst, L), L > 0,
      (Args=[Fn,Init] -> true; Args=[Fn], Init=nil) },
    method('%fold', Lst, [Fn, Init], Result).

method('%fold', [], [_Fn, Acc], Acc) --> [].
method('%fold', [V|Vs], [Fn, Acc], Result) -->
    eval_call(Fn, [Acc, V], Res0),
    method('%fold', Vs, [Fn, Res0], Result).

method(do, Lst, [Fn], Result) -->
    { (is_list(Lst); Lst=nil), ! },
    foldfn(Lst, Fn, nil, nil, do_, '=', Result).
method(do, map(M), [Fn], Result) -->
    { map_pairs(map(M), KVs) },
    method('%do', KVs, [Fn], Result).
method('%do', [], _, nil) --> [].
method('%do', [K-V|KVs], [Fn], nil) -->
    eval_call(Fn, [K,V], _Result),
    method('%do', KVs, [Fn], nil).



method(filter, [], _, []) --> [].
method(filter, [H|T], [Fn], Result) -->
    eval_call(Fn, [H], Include),
    method(filter, T, [Fn], Rest),
    { \+ falsy(Include) -> Result=[H|Rest]; Result=Rest }.

method(call, Fn, Args, Result) -->
    eval_call(Fn, Args, Result).

method(use, Lib, [], nil) -->
    { file_codes(Lib, Codes),
      once(phrase(statements(Stmts), Codes)) },
    eval_stmts(Stmts, nil, _).

method(eval, Code, [], Result) -->
    { once(phrase(statements(Stmts), Code))
      -> true
      ; string_codes(Str, Code),
        err('Eval error, parsing failed: "~w"', [Str]) },
    eval_stmts(Stmts, nil, Result).

method(match, [], [[]], []) --> [].
method(match, [Spec|Specs], [Input], Out) -->
    { is_callable(Spec) },
    eval_call(Spec, [Input], MatchResult),
    method('%match', Specs, MatchResult, Out), !.

% If previous match method call failed, catch it and return nil
method(match, [Spec|_Specs], [_Input], nil) -->
    [],
    { is_callable(Spec) }.

method(match, [Spec|Specs], [Input], Result) -->
    { is_list(Spec), append(Spec, Rest, Input) },
    method(match, Specs, [Rest], Result).
method(match, [Spec|_], [Input], nil) -->
    { is_list(Spec), \+ append(Spec, _, Input) }.

% Don't continue if no match, result of whole match is nil
method('%match', _, nil, nil) --> [].
method('%match', Specs, [R|Rest], Out) -->
    method(match, Specs, Rest, Result),
    { Result = nil
      -> Out = nil
      ; (R = nil
        -> Out=Result
        ; append([R], Result, Out)) }.


method(Name, My, Args, Result) -->
    { class(My, C),
      get_record_method(C, Name, Fun, Args, Args1) },
    eval_call_my(Fun, My, Args1, Result).


% Any pure Prolog method, that doesn't need DCG evaluation context
method(Method, Me, Args, Result) -->
    [],
    {  method(Method, Me, Args, Result) }.

method(digit, N, [], nil) :- \+ between(48, 57, N), !.
method(digit, N, [], D) :- between(48,57,N), !, D is N - 48.
method(print, Me, _, Me) :- outputln(Me), !.
method(sum, Lst, [], Result) :- sum_list(Lst, Result), !.
method(first, [H|_], [], H) :- !.
method(first, [], [], nil) :- !.
method(last, Lst, [], Last) :- last(Lst, Last), !.
method(nth, Lst, [N], Nth) :- nth0(N, Lst, Nth), !.
method(lines, File, [], Lines) :- file_lines(File, Lines).
method(contents, File, [], Contents) :- file_codes(File, Contents).
method(heads, [], [], []) :- !.
method(heads, [H|T], [], [[H|T]|Heads]) :- method(heads, T, [], Heads), !.
method(reverse, Lst, [], Rev) :- reverse(Lst,Rev), !.
method(to, From, [To], Lst) :- findall(N, between(From,To,N), Lst), !.
method(to, From, [To, _], []) :- From > To, !.
method(to, From, [To, Inc], [From|Rest]) :- From1 is From + Inc, method(to, From1, [To, Inc], Rest), !.
method(fmt, PatternCs, Args, Out) :- fmt(PatternCs, Args, Out), !.
method(join, Lst, [Sep], Out) :-
    foldl({Sep}/[Item,Acc,Out]>>append([Acc, Sep, Item], Out),
          Lst, [], Intermediate),
    % Remove the separator before first element
    append(Sep, Out, Intermediate), !.
method(split, Lst, [Sep], Result) :-
    (once(append([Start, Sep, Rest], Lst))
    -> (method(split, Rest, [Sep], RestSplit),
        Result=[Start|RestSplit])
    ; Result=[Lst]), !.
method(len, nil, [], 0) :- !.
method(len, [], [], 0) :- !.
method(len, [L|Lst], [], Result) :- length([L|Lst], Result), !.
method(len, map(M), [], Result) :- map_size(map(M), Result), !.
method(at, map(M), [Key], Result) :- map_get(map(M), Key, Result), !.
method(at, map(M), [Key, Default], Result) :-
    map_get(map(M), Key, Val),
    (Val = nil
    -> Result = Default
    ; Result = Val).
method(put, map(M), [Key, Val | KVs], R) :-
    map_put(map(M), Key, Val, map(M1)),
    method(put, map(M1), KVs, R), !.
method(put, map(M), [], map(M)) :- !.
method(del, map(M), Keys, map(M1)) :-
    map_del(map(M), Keys, map(M1)), !.
method(keys, map(M), [], Ks) :-
    map_keys(map(M), Ks), !.

% Record fields act as getter
method(Field, rec(Record,ID), [], Val) :- record_get(rec(Record,ID), Field, Val), !.

% Record fields with 1 parameter act as setter
method(Field, rec(Record,R0), [Val], rec(Record,R1)) :-
    record_set(rec(Record,R0), Field, Val, rec(Record,R1)), !.

method('number?', N, [], Result) :- (number(N) -> Result=true; Result=false), !.
method('list?', L, [], Result) :- (is_list(L) -> Result=true; Result=false), !.
method('map?', M, [], Result) :- (M=map(_) -> Result=true; Result=false), !.

method(floor, N, [], Result) :- Result is floor(N).
method(ceil, N, [], Result) :- Result is ceil(N).
method(round, N, [], Result) :- Result is round(N).

method(not, false, [], true) :- !.
method(not, nil, [], true) :- !.
method(not, X, [], false) :- \+ falsy(X), !.

method('starts?', Lst, [Prefix], Result) :-
    (append(Prefix, _, Lst) -> Result=true; Result=false), !.
method('ends?', Lst, [Suffix], Result) :-
    (append(_, Suffix, Lst) -> Result=true; Result=false), !.
method(inc, N, [], Result) :- Result is N + 1.
method(dec, N, [], Result) :- Result is N - 1.
method(abs, N, [], Result) :- Result is abs(N).
method(max, [N|Ns], [], Result) :- max_list([N|Ns], Result).
method(min, [N|Ns], [], Result) :- min_list([N|Ns], Result).
method(sort, [N|Ns], [], Result) :- msort([N|Ns], Result).
method(sortu, [N|Ns], [], Result) :- sort([N|Ns], Result).
method(take, Lst, [N], Result) :- take(Lst, N, Result).
method(drop, Lst, [N], Result) :- drop(Lst, N, Result).
method(debug, X, [], X) :- debug, !.
method('_0', [N|_], [], N).
method('_1', [_,N|_], [], N).
method('_2', [_,_,N|_], [], N).
method('_3', [_,_,_,N|_], [], N).
method('nil?', V, _, B) :- (V=nil -> B=true; B=false).
method(ref, V, [], Ref) :- ref_new(Ref, V).
method(val, ref(ID), [], Val) :- ref_get(ref(ID), Val).
method(val, ref(ID), [Val], Val) :- ref_set(ref(ID), Val).
method('in?', Candidate, [Lst], Val) :-
    memberchk(Candidate, Lst) -> Val = true; Val = false.
method('has?', Lst, [Candidate], Val) :-
    memberchk(Candidate, Lst) -> Val = true; Val = false.
method(index, Lst, [Candidate], -1) :-
    \+ memberchk(Candidate, Lst), !.
method(index, Lst, [Candidate], Idx) :-
    once(append([Before, [Candidate], _], Lst)),
    length(Before, Idx).
method(read, Lst, [], [Val, Rest]) :-
    phrase(value(val(Val)), Lst, Rest).
method(else, Me, [Or], Val) :-
    falsy(Me)
    -> Val=Or
    ; Val=Me.
method(ws, Lst, [], [nil,Result]) :-
    phrase(blanks, Lst, Result).
method(part, [], _, []) :- !.
method(part, Lst, [Size], [Lst]) :- length(Lst, L), L < Size, !.
method(part, Lst, [Size], [First|Lists]) :-
    length(First, Size),
    append(First, Rest, Lst),
    method(part, Rest, [Size], Lists).
method(part, Lst, [Size,_Skip], []) :- length(Lst, L), L < Size, !.
method(part, Lst, [Size, Skip], [First|Lists]) :-
    length(First, Size),
    append(First, _, Lst),
    length(Drop, Skip),
    append(Drop, Rest, Lst),
    method(part, Rest, [Size,Skip], Lists).
method('empty?', [], [], true) :- !.
method('empty?', nil, [], true) :- !.
method('empty?', map(M), [],  true) :- rb_new(M0), M0=M, !.
method('empty?', _, [], false) :- !. % anything else is not
method(pr, X, [], X) :-
    prettyln(X), !.
method(str, N, [], Str) :- number(N), atom_codes(N, Str), !.
method(str, [], [], `[]`) :- !.
method(str, [X], [], Str) :- method(str, X, [], XStr), append([`[`, XStr, `]`], Str), !.
method(str, Lst, [], Str) :- is_list(Lst), maplist([X,S]>>method(str, X, [], S), Lst, Strs),
                             join(`, `, Strs, Vals), append([`[`, Vals, `]`], Str).
method(allpairs, Lst, [], Pairs) :-
    findall([A,B], (select(A, Lst, Rest), member(B, Rest)), Pairs).
method(sign, N, [], Sign) :- number(N), sign(N, Sign).
method(numdigits, N, [], Digits) :- atom_length(N, Digits).
method(pow, Num, [N], Res) :- Res is Num ^ N.
method('is?', rec(T,_), [record_ref(T)], true) :- !.
method('is?', _, _, false) :- !.
method('as-list', map(M), [], Result) :-
    map_pairs(map(M), Pairs),
    maplist([K-V,[K,V]]>>true, Pairs, Result).
method('as-map', Lst, [], map(M)) :-
    maplist([[K,V],K-V]>>true, Lst, Pairs),
    map_pairs(map(M), Pairs).
% for putting a breakpoint
debug.

% add all methods here
method(if/1, "if(Then)\nIf recipient is truthy, return Then. If Then is a function, it is called with the recipient as argument and its return value is returned.").
method(if/2, "if(Then,Else)\nIf recipient is truthy, return Then otherwise return Else. Both can be values or functions.").
method(cond/_, "cond(Action1,...,ActionN, Else)\nTakes the first truthy value in recipient and runs the corresponding action or Else if no value is truthy. If Else is not specified, returns nil when no value is truthy.").
method(keep/1,"keep(Fn)\nCall Fn on all values in recipient, returns list of all results that are not nil.").
method(map/1,"map(Fn)\nCall Fn on all values in recipient and returns values as a list.").
method(mapcat/1,"mapcat(Fn)\nCall Fn on all values in recipient appending all resulting lists in one list. Fn must return a list or nil.").
method(group/1,"group(Fn)\nGroup values in recipient by Fn. Calls Fn on each value in recipient and returns map with the result as key and list of values having that result as value.").
method(print/0,"Print value to standard output.").
method(digit/0,"Return ASCII digit code as number, or nil if code is not a digit.").
method(do/1,"do(Fn)\nCall Fn on each value in recipient, presumably for side effects. For maps, Fn is called with key and value as arguments. Returns nil.").
method(sum/0,"Returns sum of all values in recipient.").
method(filter/1,"filter(Fn)\nCall Fn on each value in recipient, return list of values where the result is truthy.").
method(call/_,"Call recipient function with given arguments.").
method(first/0,"Return first value in recipient.").
method(last/0,"Return last value in recipient.").
method(nth/0,"nth(N)\nReturn the Nth value in recipient.").
method(lines/0,"Return list of lines in file.").
method(heads/0,"Return successive heads of recipient.").
method(reverse/0,"Return recipient reversed.").
method(to/1,"to(Upto)\nReturn list of successive numbers from recipient to Upto.").
method(to/2,"to(Upto, Skip)\nReturn list of successive numbers from recipient to Upto, incrementing with Skip.").
method(fmt/_,"fmt(Arg1,...,ArgN)\nFormat recipient string with given parameters, any use \"%s\" and \"%d\" to mark argument strings and numbers.").
method(join/1,"join(Sep)\nJoin recipient list of lists into one list with separator.").
method(split/1,"split(Sep)\nsplit recipient list into sublists at each occurence of separator Sep.").
method(len/0,"Returns the length of a list.").
method(at/1,"at(Key)\nReturn value at key or nil if not present.").
method(put/_,"put(Key1,Val1,...,KeyN,ValN)\nPut values into map, returns new map with added mappings.").
method('number?'/0,"True if recipient is a number, false otherwise.").
method('list?'/0,"True if recipient is a list, false otherwise.").
method('map?'/0,"True if recipient is a map, false otherwise.").
method(floor/0,"Return largest integer that is less than or equal to recipient.").
method(ceil/0,"Return smallest integer that is greater than or equal to recipient.").
method(round/0,"Return the closest integer to recipient.").
method(not/0,"Return negated boolean of recipient.").
method('starts?'/1,"starts?(Prefix)\nReturns true if recipient starts with Prefix, false otherwise.").
method('ends?'/1,"ends(Suffix)\nReturns true if recipient ends with Suffix, false otherwise.").
method(some/1,"some(Pred)\nReturns the first value in recipient where the result of calling Pred on it returns truthy, or nil otherwise.").
method(inc/0,"Return the recipient incremented by 1.").
method(dec/0,"Return the recipient decremented by 1.").
method(abs/0,"Return the absolute value of recipient.").
method(min/0,"Return the minimum value of a list of numbers.").
method(max/0,"Return the maximum value of a list of numbers.").
method(min/1,"min(Fn)\nCall Fn on each value in recipient, return the minimum value.").
method(max/1,"max(Fn)\nCall Fn on each value in recipient, return the maximum value.").
method(sort/0,"Return recipient as sorted.").
method(sortu/0,"Return recipient as sorted without duplicates.").
method(drop/1,"drop(N)\nReturn recipient without the first N values.").
method(take/1,"take(N)\nReturn the first N values of recipient.").
method(debug/0,"Print internal interpreter state.").
method('_0'/0,"Return 1st value of recipient."). % first
method('_1'/0,"Return 2nd value of recipient."). % second
method('_2'/0,"Return 3rd value of recipient."). % third
method('_3'/0,"Return 3rd value of recipient."). % fourth
method(sort/1,"sort(Fn)\nCall Fn on each value in recipient, return recipient sorted by the return values.").
method(minw/1,"minw(Fn)\nCall Fn on each value in recipient, return list containing the smallest result and the value it corresponds with.").
method(maxw/1,"maxw(Fn)\nCall Fn on each value in recipient, return list containing the largest result and the value it corresponds with.").
method(use/1, "Load file denoted by recipient as elf code.\nAll record types, methods and names defined in the file are available after this call.").
method(fold/2, "fold(Fn, Init)\nCall Fn with Init and first value of recipient, then with successive return values and values in recipient until all values are processed. If Init is omitted, nil is used.").
method('nil?'/0, "True if recipient is nil, false otherwise.").
method(eval/0, "Eval recipient as elf code, returns the last value.").
method(ref/0, "Create new mutable reference from recipient.").
method(val/_, "Get or set current value of mutable reference.").
method(swap/_, "swap(Fn,...Args)\nUpdate value of ref by calling Fn on its current value (and optional Args). Returns new value.").
method(while/1, "while(Then)\nCall recipient fn repeatedly while it returns a truthy value, call Then with that value.").
method('in?'/1, "in?(List)\nTrue if recipient is a member of List, false otherwise.").
method('has?'/1, "has?(Item)\nTrue if Item is member of recipient list, false otherwise.").
method('takew'/1, "takew(Pred)\nReturn items of recipient list while calling Pred on item returns truthy.").
method('dropw'/1, "dropw(Pred)\nReturn items of recipient after the first Pred call on item returns falsy.").
method('splitw'/1, "splitw(Pred)\nCombines takew and dropw. Return list of [taken, dropped].").
method(read/0, "Read an Elf value (number, string, boolean or nil) from recipient string. Returns a list containing the read value and the rest of the string.").
method(else/1, "else(V)\nReturn recipient value if it is truthy, otherwise return V.").
method(ws/0, "Parsing method to skip 0 or more whitespace characters. Returns [nil, rest] where rest is the string after the whitespace.").
method(match/1, "match(Input)\nParse input by matching it to specs in recipient list. Each spec can be a list expected at this position or a function that parses from the current position. Parsing function must return a parsed value and the rest of the list. Returns list of all non-nil values parsed by parsing functions.").
method(part/_, "part(Size,[Skip])\nPartition list into sublists of Size. If skip is omitted it is the same as Size. If skip is omitted, the last list may be shorter than the others if the input is not evenly sized.

Example:
[1,2,3,4] part(2,1)
=> [[1,2],[2,3],[3,4]]
").
method(count/_, "count(Fn)\nCount the number of values in recipient where Fn returns truthy.").
method('empty?'/0, "True if recipient is nil, the empty list or an empty map.").
method('contents'/0, "Return contents of recipient file as string.").
method(index/1, "index(Elt)\nReturns the first index of Elt in recipient list or -1 if it does not appear in the list.").
method(pr/0, "Pretty print recipient. Returns recipient.").
method(str/0, "Return readable representation of recipient as string.").
method(allpairs/0, "Return all possible [A,B] lists where A and B are elements of recipient list.").
method(sign/0, "Return -1, 0 or 1 if recipient is negative, zero or positive respectively.").
method(numdigits/0, "Return the number of digits in a number, eg. 123 => 3").
method(pow/1, "pow(N)\nRaise recipient to Nth power.").
method('is?'/1, "is?(RecordType)\nTrue iff recipient is an instance of RecordType.").
method('as-list'/0, "Return recipient map as list of [k,v] entries.").
method('as-map'/1, "Return recipient list of [k,v] entries as a map.").

falsy(nil).
falsy(false).

% List accumulator goals
keep_(Lst, nil, Lst) :- !.
keep_(Lst, false, Lst) :- !.
keep_(Lst, X, [X|Lst]) :- !.
map_(Lst, Item, [Item|Lst]) :- !.
do_(_, _, nil) :- !.
count_(Acc, nil, Acc) :- !.
count_(Acc, false, Acc) :- !.
count_(Acc, _, Acc1) :- succ(Acc,Acc1).
min_(nil, X, X) :- !.
min_(L, R, Out) :- (L < R -> Out=L;Out=R), !.
max_(nil, X, X) :- !.
max_(L, R, Out) :- (L > R -> Out=L;Out=R), !.

% min and max with witness
minw_(nil, X, X) :- !.
minw_(R0-W0, R1-_, R0-W0) :- R0 < R1, !.
minw_(R0-_, R1-W1, R1-W1) :- R0 >= R1, !.

maxw_(nil, X, X) :- !.
maxw_(R0-W0, R1-_, R0-W0) :- R0 > R1, !.
maxw_(R0-_, R1-W1, R1-W1) :- R0 =< R1, !.

pair_list(L-R, [L, R]) :- !.




% take&drop utils
take([], _, []) :- !.
take(_, 0, []) :- !.
take([X|Xs], N, [X|Result]) :-
    N > 0, succ(N1, N),
    take(Xs, N1, Result).

drop([], _, []) :- !.
drop(L, 0, L) :- !.
drop([_|Xs], N, Drop) :-
    N > 0, succ(N1, N),
    drop(Xs, N1, Drop).


%% Top level runner interface

initial_ctx(ctx(env{},[],nil)).

exec(Stmts, Out) :-
    record_cleanup,
    ref_cleanup,
    initial_ctx(Ctx),
    once(phrase(eval_stmts(Stmts,nil,Out), [Ctx], _)).

exec(Stmts, Out, ctx(E,A,P), CtxOut) :-
    phrase(eval_stmts(Stmts, P, Out), [ctx(E,A,P)], [CtxOut]).

run(File) :-
    once(phrase_from_file(statements(Stmts), File)),
    exec(Stmts, _Out).

main :-
    current_prolog_flag(argv, []),
    repl.
main :-
    current_prolog_flag(argv, [Script]),
    run(Script).

run_codes(Input, Out) :-
    %call_with_time_limit(1, (phrase(statements(Stmts), Input))),
    once(phrase(statements(Stmts), Input)),
    once(exec(Stmts,Out1)),
    format_result(Out1, Out).

format_result(rec(Rec,ID), Out) :- record_dict(rec(Rec,ID), Out), !.
format_result(map(ID), Out) :- map_pairs(map(ID), Out), !.
format_result(Out, Out).

run_string(Input, Out) :-
    string_codes(Input, Codes),
    run_codes(Codes, Out).

run_string_pretty(Input, Out) :-
    run_string(Input, Result),
    with_output_to(string(Out),
                   prettyln(Result)).

repl_input(Codes) :-
    read_line_to_codes(user_input,Codes),
    (Codes = end_of_file -> halt; true).

repl :-
    record_cleanup,
    initial_ctx(Ctx0),
    State = state(Ctx0),
    version(V),
    format('Welcome to Elf REPL (v~w).\nExit with Ctrl-d.\nNo one is coming to help you... have fun!\n\n', [V]),
    repeat,
    format("elf> ",[]),
    repl_input(Codes),
    (phrase(elf:statements(Stmts), Codes)
    -> (arg(1, State, ctx(E,A,P)),
        ((catch(exec(Stmts, Result, ctx(E,A,P), ctx(E1,_,_)),
                _Err,
                fail))
         -> (with_output_to(string(Out), once(prettyln(Result))),
             format('~w\n', [Out]),
             nb_setarg(1, State, ctx(E1,[],Result)))
        ; format('Execution failed ¯\\_(ツ)_/¯\n',[])),
        fail)
    ; (format('Parse error (╯°□°)╯︵ ┻━┻\n', []), fail)).


:- begin_tests(elf).
:- use_module(examples, [ex/3]).

prg("foo: \"jas6sn0\", foo keep(&digit)", [6,0]).
prg("[4, 2, 0, 6, 9] sum", 21).
prg("[6, 2] sum * 4", 32).
prg("\"README.md\" lines first", `# Elf programming language`).
prg("[1, 2, 3, 4] map({$ * 2})", [2, 4, 6, 8]).
prg("[1, 2, 3, 4] heads", [[1,2,3,4],[2,3,4],[3,4],[4]]).
prg("\"elf\" reverse", `fle`).
prg("\"some4digt02here\" keep(&digit), (_ first * 10) + _ last", 42).
prg("1 to(5) reverse", [5,4,3,2,1]).
prg("42 to(69, 7)", [42,49,56,63]).
prg("\"%s day with %d%% chance of rain\" fmt(\"sunny\",7)",
    `sunny day with 7% chance of rain`).
prg("[\"foo\", \"bar\" ,\"baz\"] join(\" and \")",
    `foo and bar and baz`).
prg("\"foo, quux, !\" split(\", \")",
    [`foo`, `quux`, `!`]).
prg("(\"foo\" ++ \"bar\") len", 6).
prg("([\"foo\" len] first > 1) if(\"big\")", `big`).
prg("Elf{age}, e: Elf{age:665}, e age(e age + 1)", 'Elf'{age:666}).
prg("[\"foo\",\"bar\",\"other\"] group(&len) at(5)", [`other`]).
prg("m: %{\"foo\": 40} put(\"bar\",2), m at(\"foo\") + m at(\"bar\")", 42).
prg("{a,b| a + b} call(40,2)", 42).
prg("-42 abs", 42).
prg("69 inc", 70).
prg("-69 dec", -70).

prg("[100,200] nth(0)", 100).
prg("[100,200] reverse", [200,100]).
prg("[\"foo\",\"bar\"] join(\" and \")", `foo and bar`).
prg("\"foo&bar\" split(\"&\")", [`foo`,`bar`]).
prg("\"foobar\" starts?(\"foo\")", true).
prg("\"foobar\" starts?(\"bra\")", false).
prg("\"foobar\" ends?(\"bar\")", true).
prg("\"foobar\" ends?(\"asd\")", false).
prg("[9,1,420,-6] max", 420).
prg("[9,1,420,-6] min", -6).
prg("[5,6,7,1,2,5] sort", [1,2,5,5,6,7]).
prg("[5,6,7,1,2,5] sortu", [1,2,5,6,7]).
prg("[false,false,true,true] cond(1,2,3,4,5)", 3).
prg("[false,false,false,false] cond(1,2,3,4,5)", 5).
prg("[\"im\",\"so\",\"meta\",\"even\",\"this\",\"acronym...\"] mapcat({$ take(1)})",
    `ismeta`).
prg("\"foobar\" drop(3)", `bar`).
prg("[[11,22,33],[44,55,66]] min(&first)", 11).
prg("[[11,22,33],[44,55,66]] max(&first)", 44).
prg("n: 5 ref, n swap(&inc)", 6).
prg("n: 20 ref, n swap({a,b| a + b}, 22)", 42).
prg("l: [] ref, n: 5 ref, { n swap(&dec) > 0 } while({ l val(l val ++ [n val]) }), l val",
    [4,3,2,1]).
prg("\"examples/elves.elf\" use, elves max(&age)", 317).
prg("\"examples/elves.elf\" use, elves sort(&age) _0 age", 75).
prg("\"examples/elves.elf\" use, elves minw(&age) _1 name", `Biscuit Peppermint`).
prg("\"examples/elves.elf\" use, elves maxw(&age) first", 317).
prg("\"examples/elves.elf\" use, elves minw(&age) _1 greet(\"world\")",
    `Hi world! My name is Biscuit Peppermint and I'm 75 years old!`).
prg("%{\"foo\": 41, \"bar\": 665} map(&inc) at(\"foo\")", 42).
prg("[4,6,32] fold({$1 + $2},0)", 42).
prg("[\"a\",\"b\",\"c!\"] fold({a,b| a ++ b})", `abc!`).
prg("n: 5, \"x: 11, n * x\" eval", 55).

prg("\"24dec\" splitw(&digit)", [`24`, `dec`]).
prg("\"24dec\" read", [24, `dec`]).
prg("[&read, \"-\", &read, \"-\", &read] match(\"2024-12-24\")", [2024,12,24]).
prg("[\"mul\", &read] match(\"mul420\")", [420]).
prg("[\"mul\", &read] match(\"mil420\")", nil).
prg("[\"mul\", &read] match(\"mulx20\")", nil).
prg("n: nil, n else(10)", 10).
prg("n: 42, n else(123)", 42).
prg("[1,2,3] all?({$ > 0})", true).
prg("[1,0,3] all?({$ > 0})", false).
prg("[] all?(&print)", true). % vacuous truth
prg("[[1,2],[5,1],[10,42]] map(&<)", [true,false,true]).
prg("\"hello\" part(2,1)", [`he`,`el`,`ll`,`lo`]).
prg("\"hello\" part(2)", [`he`,`ll`,`o`]).
prg("[] part(666)", []).
prg("\"4digit20here\" count(&digit)", 3).
prg("[] empty?", true).
prg("[1] empty?", false).
prg("%{} empty?", true).
prg("%{\"foo\": 42} empty?", false).
prg("42 empty?", false).
prg("[\"hat\", \"fat\", \"cat\"] keep(`(c|h)at`)", [[`hat`,`h`], [`cat`,`c`]]).
prg("\"hello\" index(@l)", 2).
prg("[42,100] index(666)", -1).
prg("start: \"start\", %{start: 666, \"end\": 1234}, _ at(\"start\")", 666).
prg("[42,100] str", `[42, 100]`).
prg("666 str", `666`).
prg("Foo{a,b}, Foo{a:2} is?(Foo)", true).
prg("Foo{a,b}, 42 is?(Foo)", false).
prg("%{\"foo\": 42} as-list", [[`foo`,42]]).
prg("[[\"foo\", 42], [\"bar\", 666]] as-map at(\"bar\")", 666).
prg("Number.sq: {my * my}, 42 sq", 1764).
prg("List.frob: {my drop(1) ++ [my _0]}, \"Hello\" frob frob", `lloHe`).
err("n: 5, \"x: 11, n* \" eval", err('Eval error, parsing failed: "~w"', _)).
err("[1,2,3] scum", err('Method call failed: ~w/0', [scum])).
err("[1,2,3] mab(&inc)", err('Method call failed: ~w/~w', [mab, 1])).

test(programs, [forall(prg(Source,Expected))]) :-
    once(run_string(Source,Actual)),
    Expected = Actual.


test(examples, [forall(ex(_Name,Source,Expected))]) :-
    once(run_string(Source,Actual)),
    Expected = Actual.

test(errors, [forall(err(Code, Error))]) :-
    catch(once(run_string(Code,_)),
          Err,
          Error=Err).

:- end_tests(elf).
