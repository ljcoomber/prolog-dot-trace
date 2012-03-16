% TODO: Find way to match variable names and verify calls with ungrounded vars
% TODO: Do not show 'local' args (from callable clauses?)
% TODO: Put in finer grain tests to compensate for the above, though very hard
%       when dealing with frame vars!

%:- use_module(library(readutil)).
:- use_module(dot_trace).
:- use_module('lib/dot-dcg/dot_dcg').

% Must use modified portray_text module so that quotes are escaped for dot
:- use_module('lib/swi-prolog/portray_escaped_text').
:- set_portray_text(min_length, 1).

:- begin_tests(dot_trace).

test(p_simple) :-
    % Stricter testing is not possible due to ungrounded vars 
    do_trace(p_simple, Statements),

    find_node(Start, '"Start"', Statements),
    find_node(PSimple, '"p_simple/0"', Statements),

    find_edge(Start, PSimple, '"0: p_simple"', Statements),
    find_edge(PSimple, Start, '"9 "', Statements).

test(p_backtrack, [nondet]) :-
    do_trace(p_backtrack, Statements),

    find_node(Start, '"Start"', Statements),
    find_node(PBacktrack, '"p_backtrack/0"', Statements),

    find_edge(Start, PBacktrack, '"1: p_backtrack"', Statements),
    find_edge(PBacktrack, Start, '"19 "', Statements).

test(p_cut) :-
    do_trace(p_cut, _Statements),
    find_node(Start, '"Start"', Statements),
    find_node(Pred, '"p_cut/0"', Statements),

    find_edge(Start, Pred, '"1: p_cut"', Statements),
    find_edge(Pred, Start, '"23 "', Statements).
        
test(p_ifthen) :-
    do_trace(p_ifthen, _Statements),

    find_node(Start, '"Start"', Statements),
    find_node(Pred, '"p_ifthen/0"', Statements),

    find_edge(Start, Pred, '"1: p_ifthen"', Statements),
    find_edge(Pred, Start, '"8 "', Statements).
    
test(p_error) :-
    do_trace(p_error, _Statements),

    find_node(Start, '"Start"', Statements),
    find_node(Pred, '"p_error/0"', Statements),

    find_edge(Start, Pred, '"1: p_error"', Statements),
    find_edge(Pred, Start, '"14 "', Statements).
    
:- end_tests(dot_trace).

do_trace(Pred, Statements) :-
    atomic_list_concat(['tmp/', Pred, '.dot'], File),

    setup_call_cleanup(open(File, write, WStream),
                       once(dot_trace:dot_trace_stream(WStream, Pred)),
                       close(WStream)),
    
    setup_call_cleanup(open(File, read, RStream),
                       (read_stream_to_codes(RStream, Result),
                        dot_dcg:graph(digraph(_, Statements), Result, [])),
                       close(RStream)).

find_edge(Start, End, Label, Statements) :-
    member(edge([Start, End], Attrs), Statements),
    member(attr(label,Label), Attrs), !.
    
find_node(Id, Label, Statements) :-
    member(node_stmt(Id, Attrs), Statements),
    member(attr(label, Label), Attrs),
    !.

% The following is taken from the SWI Prolog test file test_trace_callback.pl

p_simple :-
	app("ab", "cd", X),
	is_list(X).

p_backtrack :-
	app(_, Y, "hello"),
	Y = [0'l|_].

p_cut :-
	app(_, Y, "hello"),
	Y = [0'l|_], !,
	a.

p_ifthen :-
	(   no
	->  true
	;   a
	).

p_error :-
	catch(error, _, true).

error :-
	a, b, e, c.

a.
b.
c.

no :-
	fail.
e :-
	X is 1/0,
	number(X).

app([], L, L).
app([H|T], L, [H|L2]) :-
	app(T, L, L2).
