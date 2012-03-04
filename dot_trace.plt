%:- use_module(library(readutil)).
:- use_module(dot_trace).
:- use_module('lib/dot-dcg/dot_dcg').

:- begin_tests(dot_trace).

test(simple) :-
    do_trace(p_simple, Graph),
    Graph = digraph(_, Statements),

    find_node(Start, '"Start"', Statements),
    find_node(PSimple, '"p_simple"', Statements),
    find_node(Cond, '"system: (2>1)"', Statements),

    find_edge(Start, PSimple, '"0"', Statements),
    find_edge(PSimple, Cond, '"1"', Statements),
    find_edge(PSimple, Start, '"2  Exit: p_simple"', Statements).

:- end_tests(dot_trace).

% TODO: setup_call_cleanup
% TODO: revert to proper temp file
do_trace(Pred, Graph) :-
    %tmp_file_stream(text, File, WStream),
    open('tmp.pl', write, WStream),
    dot_trace:dot_trace_stream(WStream, Pred),
    close(WStream),
    open('tmp.pl', read, RStream),
    read_stream_to_codes(RStream, Result),
    close(RStream),               
    %delete_file(File),
    dot_dcg:graph(Graph, Result, []).

find_edge(Start, End, Label, Statements) :-
    member(edge([Start, End], [attr(label,Label)]), Statements), !.
    
find_node(Id, Label, Statements) :-
    member(node_stmt(Id, [attr(label,Label)]), Statements), !.

p_simple :-
    2 > 1.
