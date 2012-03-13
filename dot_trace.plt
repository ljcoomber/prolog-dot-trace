% TODO: Find way to match variable names and verify calls with ungrounded vars
% TODO: Put in finer grain tests to compensate for the above

%:- use_module(library(readutil)).
:- use_module(dot_trace).
:- use_module('lib/dot-dcg/dot_dcg').

% Must use modified portray_text module so that quotes are escaped
:- use_module('lib/swi-prolog/portray_escaped_text').
:- set_portray_text(min_length, 1).

:- begin_tests(dot_trace).

test(p_simple) :-
    % Stricter testing is not possible with ungrounded vars 
    do_trace(p_simple, _Statements),

    find_node(Start, '"Start"', Statements),
    find_node(PSimple, '"p_simple"', Statements),
    find_node(IsList, '"system:is_list([97,98,99,100])"', Statements),

    find_edge(Start, PSimple, '"0"', Statements),
    find_edge(PSimple, App1, '"1"', Statements),
    find_edge(App1, App2, '"2"', Statements),
    find_edge(App2, _, '"3"', Statements),
    find_edge(App1, PSimple, _, Statements),
    find_edge(PSimple, IsList, '"7"', Statements),
    find_edge(IsList, PSimple, '"8"', Statements),
    find_edge(PSimple, Start, '"9"', Statements).

test(p_backtrack, [nondet]) :-   
    do_trace(p_backtrack, _Statements),

    find_node(Start, '"Start"', Statements),
    find_node(PBacktrack, '"p_backtrack"', Statements),

    find_edge(Start, PBacktrack, '"1"', Statements),
    find_edge(PBacktrack, App1, '"2"', Statements),
    find_edge(PBacktrack, App1, '"6"', Statements),

    find_edge(U1, U2, '"4"', Statements),
    find_edge(U2, U1, '"5"', Statements),

    find_edge(U3, U4, '"10"', Statements),
    find_edge(U4, U3, '"11"', Statements).

%test(p_cut) :-
%    do_trace(p_cut, _Statements).
    
%test(p_ifthen, blocked(todo)) :-
%    do_trace(p_ifthen, _Statements).

%test(p_error, blocked(todo)) :-
%    do_trace(p_error, _Statements).    

:- end_tests(dot_trace).

do_trace(Pred, Statements) :-
    atomic_list_concat(['tmp/', Pred, '.dot'], File),

    setup_call_cleanup(open(File, write, WStream),
                       dot_trace:dot_trace_stream(WStream, Pred),
                       close(WStream)),

    setup_call_cleanup(open(File, read, RStream),
                       (read_stream_to_codes(RStream, Result),
                        dot_dcg:graph(digraph(_, Statements), Result, [])),
                       close(RStream)).


find_edge(Start, End, Label, Statements) :-
    member(edge([Start, End], Attrs), Statements),
    member(attr(label,Label), Attrs), !.
    
find_node(Id, Label, Statements) :-
    member(node_stmt(Id, [attr(label,Label)]), Statements), !.


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
